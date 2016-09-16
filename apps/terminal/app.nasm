%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/string.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_label.inc'
%include 'class/class_string.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream_msg_out.inc'
%include 'class/class_stream_msg_in.inc'
%include 'class/class_master.inc'

	fn_function apps/terminal/app

		buffer_size equ 120

		def_structure shared
			ptr shared_master, shared_panel, shared_history, shared_window
			ulong shared_history_index
			pubyte shared_bufp
			struct shared_buffer, buffer
		def_structure_end

		struct myapp, obj
		struct shared, shared
		struct buffer, buffer
		ptr msg, stream, window_panel, label, string, mymailbox, mailbox
		ulong owner, length
		long state
		int width, height

		;init app vars
		push_scope
		slot_function class, obj
		static_call obj, init, {&myapp, @_function_}, {_}
		static_call master, create, {}, {shared.shared_master}
		static_call vector, create, {}, {shared.shared_history}
		assign {0}, {shared.shared_history_index}

		;create my window
		static_call window, create, {}, {shared.shared_window}
		static_call window, get_panel, {shared.shared_window}, {window_panel}
		static_call string, create_from_cstr, {"Terminal"}, {string}
		static_call window, set_title, {shared.shared_window, string}
		static_call string, create_from_cstr, {"Ready"}, {string}
		static_call window, set_status, {shared.shared_window, string}

		;add my app panel
		static_call flow, create, {}, {shared.shared_panel}
		static_call flow, set_flow_flags, {shared.shared_panel, flow_flag_down | flow_flag_fillw}
		static_call flow, add_back, {shared.shared_panel, window_panel}

		;add term lines to my app panel
		assign {30}, {length}
		loop_while {length}
			assign {length - 1}, {length}
			static_call label, create, {}, {label}
			static_call string, create_from_cstr, {">"}, {string}
			static_call label, set_text, {label, string}
			static_call label, set_color, {label, 0xff000000}
			static_call label, set_text_color, {label, 0xff00ff00}
			static_call label, set_font, {label, "fonts/OpenSans-Regular.ttf", 16}
			static_call label, add_back, {label, shared.shared_panel}
		loop_end

		;set to pref size
		method_call window, pref_size, {shared.shared_window}, {width, height}
		static_call window, change, {shared.shared_window, 0, 0, 640, height}

		;set window owner
		static_call sys_task, tcb, {}, {owner}
		static_call window, set_owner, {shared.shared_window, owner}

		;add to screen and dirty
		static_call gui_gui, add, {shared.shared_window}
		static_call window, dirty_all, {shared.shared_window}

		;set up term buffer
		assign {&shared.shared_buffer}, {shared.shared_bufp}

		;app event loop
		static_call sys_task, mailbox, {}, {mymailbox, _}
		loop_start
			;select on multiple mailboxes
			static_call master, select, {shared.shared_master, mymailbox}, {mailbox}

			;which mailbox has mail ?
			if {mailbox == mymailbox}
				;dispatch event to view and terminal
				static_call sys_mail, read, {mailbox}, {msg}
				method_call view, event, {msg->ev_msg_view, msg}
				if {msg->ev_msg_type == ev_type_key && msg->ev_msg_keycode > 0}
					local_call terminal_input, {&shared, msg->ev_msg_key}, {r0, r1}
				endif
				static_call sys_mem, free, {msg}
			else
				;output from a pipe element
				static_call master, get_stream, {shared.shared_master, mailbox}, {stream}
				local_call pipe_output, {&shared, stream}, {r0, r1}, {r0}, {state}
				if {state == -1}
					;EOF
					static_call master, stop, {shared.shared_master}
					static_call string, create_from_cstr, {"Ready"}, {string}
					static_call window, set_status, {shared.shared_window, string}
				endif
			endif
			static_call sys_task, yield
		loop_end

		;clean up
		static_call vector, deref, {shared.shared_history}
		static_call master, deref, {shared.shared_master}
		static_call window, deref, {shared.shared_window}
		method_call obj, deinit, {&myapp}
		pop_scope
		return

	pipe_output:
		;inputs
		;r0 = shared
		;r1 = stream
		;outputs
		;r0 = -1 if EOF

		ptr shared, stream
		long char
		ulong ready

		push_scope
		retire {r0, r1}, {shared, stream}

		loop_start
 			static_call stream, read_char, {stream}, {char}
			breakif {char == -1}
			local_call terminal_output, {shared, char}, {r0, r1}
			method_call stream, read_ready, {stream}, {ready}
		loop_until {!ready}

		eval {char}, {r0}
		pop_scope
		return

	terminal_input:
		;inputs
		;r0 = shared
		;r1 = char input

		ptr shared, string, last, stream
		ulong length, notsame
		ubyte char, state

		push_scope
		retire {r0, r1}, {shared, char}

		;echo char to terminal
		local_call terminal_output, {shared, char}, {r0, r1}

		;buffer char
		assign {char}, {*shared->shared_bufp}
		assign {shared->shared_bufp - &shared->shared_buffer}, {length}

		;send line ?
		if {char == 10 || char == 13}
			;what state ?
			static_call master, get_state, {shared->shared_master}, {state}
			if {state == stream_mail_state_stopped}
				;push new history entry if not same as last entry
				breakif {!length}
				static_call string, create_from_buffer, {&shared->shared_buffer, length}, {string}
				static_call vector, get_length, {shared->shared_history}, {shared->shared_history_index}
				if {!shared->shared_history_index}
				new_entry:
					static_call vector, push_back, {shared->shared_history, string}
					assign {shared->shared_history_index + 1}, {shared->shared_history_index}
				else
					static_call vector, ref_back, {shared->shared_history}, {last}
					static_call string, compare, {string, last}, {notsame}
					static_call string, deref, {last}
					gotoif {notsame}, new_entry
					static_call string, deref, {string}
				endif

				;start new pipe
				static_call master, start, {shared->shared_master, &shared->shared_buffer, length}
				static_call master, get_state, {shared->shared_master}, {state}
				breakif {state != stream_mail_state_started}
				static_call string, create_from_cstr, {"Busy"}, {string}
				static_call window, set_status, {shared->shared_window, string}
			else
				;feed active pipe
				static_call master, get_input, {shared->shared_master}, {stream}
				static_call stream, write, {stream, &shared->shared_buffer, length}
				static_call stream, write_char, {stream, 10}
				method_call stream, write_flush, {stream}
			endif
			assign {&shared->shared_buffer}, {shared->shared_bufp}
		elseif {char == 128}
			;backspace
			if {length}
				assign {shared->shared_bufp - 1}, {shared->shared_bufp}
			endif
		elseif {char == 129}
			;cursor up
			static_call vector, get_length, {shared->shared_history}, {length}
			breakif {!length}
			if {shared->shared_history_index}
				assign {shared->shared_history_index - 1}, {shared->shared_history_index}
			endif
			static_call vector, ref_element, {shared->shared_history, shared->shared_history_index}, {string}
			static_call sys_mem, copy, {&string->string_data, &shared->shared_buffer, string->string_length}, \
										{_, shared->shared_bufp}
			static_call stream, create, {string, 0, &string->string_data, string->string_length}, {stream}
			local_call pipe_output, {shared, stream}, {r0, r1}
		elseif {char == 130}
			;cursor down
			static_call vector, get_length, {shared->shared_history}, {length}
			assign {shared->shared_history_index + 1}, {shared->shared_history_index}
			if {shared->shared_history_index > length}
				assign {length}, {shared->shared_history_index}
			endif
			if {shared->shared_history_index == length}
				static_call string, create_from_cstr, {""}, {string}
			else
				static_call vector, ref_element, {shared->shared_history, shared->shared_history_index}, {string}
			endif
			static_call sys_mem, copy, {&string->string_data, &shared->shared_buffer, string->string_length}, \
										{_, shared->shared_bufp}
			static_call stream, create, {string, 0, &string->string_data, string->string_length}, {stream}
			local_call pipe_output, {shared, stream}, {r0, r1}
		elseif {char == 27}
			;esc
			static_call master, stop, {shared->shared_master}
			static_call string, create_from_cstr, {"Ready"}, {string}
			static_call window, set_status, {shared->shared_window, string}
			assign {&shared->shared_buffer}, {shared->shared_bufp}
		elseif {char >= 32 && char < 127}
			;next char
			assign {shared->shared_bufp + 1}, {shared->shared_bufp}
		endif
		pop_scope
		return

	terminal_output:
		;inputs
		;r0 = shared
		;r1 = char output

		ptr shared, label, string, line_string, new_line_string
		ulong length, char

		push_scope
		retire {r0, r1}, {shared, char}
		if {char == 10 || char == 13}
			;scroll lines
			static_call flow, get_first, {shared->shared_panel}, {label}
			static_call label, add_back, {label, shared->shared_panel}
			method_call flow, layout, {shared->shared_panel}
			static_call string, create_from_cstr, {">"}, {string}
			static_call label, set_text, {label, string}
			static_call flow, dirty_all, {shared->shared_panel}
		elseif {char == 128}
			;backspace
			static_call flow, get_last, {shared->shared_panel}, {label}
			static_call label, get_text, {label}, {line_string}
			static_call string, get_length, {line_string}, {length}
			if {length > 1}
				assign {length - 1}, {length}
			endif
			static_call string, create_from_buffer, {&line_string->string_data, length}, {new_line_string}
			static_call string, deref, {line_string}
			static_call label, set_text, {label, new_line_string}
			static_call label, dirty, {label}
		elseif {char == 129 || char == 130}
			;cursor up/down
			static_call flow, get_last, {shared->shared_panel}, {label}
			static_call string, create_from_cstr, {">"}, {string}
			static_call label, set_text, {label, string}
			static_call label, dirty, {label}
		elseif {char >= 32 && char < 127}
			;append char
			static_call flow, get_last, {shared->shared_panel}, {label}
			static_call string, create_from_cstr, {&char}, {string}
			static_call label, get_text, {label}, {line_string}
			static_call string, add, {line_string, string}, {new_line_string}
			static_call string, deref, {line_string}
			static_call string, deref, {string}
			static_call label, set_text, {label, new_line_string}
			static_call label, dirty, {label}
		endif
		pop_scope
		return

	fn_function_end
