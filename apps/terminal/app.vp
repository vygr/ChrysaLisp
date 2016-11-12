%include 'inc/func.ninc'
%include 'inc/gui.ninc'
%include 'inc/string.ninc'
%include 'class/class_window.ninc'
%include 'class/class_flow.ninc'
%include 'class/class_label.ninc'
%include 'class/class_string.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_stream_msg_out.ninc'
%include 'class/class_master.ninc'

def_func apps/terminal/app

	buffer_size equ 120

	def_struct shared
		ptr shared_master, shared_panel, shared_history, shared_window
		ulong shared_history_index
		pubyte shared_bufp
		struct shared_buffer, buffer
	def_struct_end

	struct myapp, obj
	struct shared, shared
	struct buffer, buffer
	ptr msg, stream, window_panel, label, string, mymailbox, mailbox
	ulong owner, length
	long state
	int width, height

	;init app vars
	push_scope
	func_path class, obj
	func_call obj, init, {&myapp, @_function_}, {_}
	func_call master, create, {}, {shared.shared_master}
	func_call vector, create, {}, {shared.shared_history}
	assign {0}, {shared.shared_history_index}

	;create my window
	func_call window, create, {}, {shared.shared_window}
	func_call window, get_panel, {shared.shared_window}, {window_panel}
	func_call string, create_from_cstr, {"Terminal"}, {string}
	func_call window, set_title, {shared.shared_window, string}
	func_call string, create_from_cstr, {"Ready"}, {string}
	func_call window, set_status, {shared.shared_window, string}

	;add my app panel
	func_call flow, create, {}, {shared.shared_panel}
	func_call flow, set_flow_flags, {shared.shared_panel, flow_flag_down | flow_flag_fillw}
	func_call flow, add_back, {shared.shared_panel, window_panel}

	;add term lines to my app panel
	assign {40}, {length}
	loop_while {length}
		assign {length - 1}, {length}
		func_call label, create, {}, {label}
		func_call string, create_from_cstr, {">"}, {string}
		func_call label, set_text, {label, string}
		func_call label, set_color, {label, 0xff000000}
		func_call label, set_text_color, {label, 0xff00ff00}
		func_call label, set_font, {label, "fonts/Hack-Regular.ttf", 16}
		func_call label, add_back, {label, shared.shared_panel}
	loop_end

	;set to pref size
	virt_call window, pref_size, {shared.shared_window}, {width, height}
	func_call window, change, {shared.shared_window, 0, 0, 640, height}

	;set window owner
	func_call sys_task, tcb, {}, {owner}
	func_call window, set_owner, {shared.shared_window, owner}

	;add to screen and dirty
	func_call gui_gui, add, {shared.shared_window}
	func_call window, dirty_all, {shared.shared_window}

	;set up term buffer
	assign {&shared.shared_buffer}, {shared.shared_bufp}

	;app event loop
	func_call sys_task, mailbox, {}, {mymailbox, _}
	loop_start
		;select on multiple mailboxes
		func_call master, select, {shared.shared_master, mymailbox}, {mailbox}

		;which mailbox has mail ?
		if {mailbox == mymailbox}
			;dispatch event to view and terminal
			func_call sys_mail, read, {mailbox}, {msg}
			virt_call view, event, {msg->ev_msg_view, msg}
			if {msg->ev_msg_type == ev_type_key && msg->ev_msg_keycode > 0}
				local_call terminal_input, {&shared, msg->ev_msg_key}, {r0, r1}
			endif
			func_call sys_mem, free, {msg}
		else
			;output from a pipe element
			func_call master, get_stream, {shared.shared_master, mailbox}, {stream}
			local_call pipe_output, {&shared, stream}, {r0, r1}, {r0}, {state}
			if {state == -1}
				;EOF
				func_call master, stop, {shared.shared_master}
				func_call string, create_from_cstr, {"Ready"}, {string}
				func_call window, set_status, {shared.shared_window, string}
			endif
		endif
		func_call sys_task, yield
	loop_end

	;clean up
	func_call vector, deref, {shared.shared_history}
	func_call master, deref, {shared.shared_master}
	func_call window, deref, {shared.shared_window}
	virt_call obj, deinit, {&myapp}
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
			func_call stream, read_char, {stream}, {char}
		breakif {char == -1}
		local_call terminal_output, {shared, char}, {r0, r1}
		virt_call stream, read_ready, {stream}, {ready}
	loop_untilnot {ready}

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
		func_call master, get_state, {shared->shared_master}, {state}
		if {state == stream_mail_state_stopped}
			;push new history entry if not same as last entry
			breakifnot {length}
			func_call string, create_from_buffer, {&shared->shared_buffer, length}, {string}
			devirt_call vector, get_length, {shared->shared_history}, {shared->shared_history_index}
			ifnot {shared->shared_history_index}
			new_entry:
				func_call vector, push_back, {shared->shared_history, string}
				assign {shared->shared_history_index + 1}, {shared->shared_history_index}
			else
				func_call vector, ref_back, {shared->shared_history}, {last}
				func_call string, compare, {string, last}, {notsame}
				func_call string, deref, {last}
				gotoif {notsame}, new_entry
				func_call string, deref, {string}
			endif

			;start new pipe
			func_call master, start, {shared->shared_master, &shared->shared_buffer, length}
			func_call master, get_state, {shared->shared_master}, {state}
			breakif {state != stream_mail_state_started}
			func_call string, create_from_cstr, {"Busy"}, {string}
			func_call window, set_status, {shared->shared_window, string}
		else
			;feed active pipe
			func_call master, get_input, {shared->shared_master}, {stream}
			func_call stream, write, {stream, &shared->shared_buffer, length}
			func_call stream, write_char, {stream, 10}
			virt_call stream, write_flush, {stream}
		endif
		assign {&shared->shared_buffer}, {shared->shared_bufp}
	elseif {char == 128}
		;backspace
		if {length}
			assign {shared->shared_bufp - 1}, {shared->shared_bufp}
		endif
	elseif {char == 129}
		;cursor up
		devirt_call vector, get_length, {shared->shared_history}, {length}
		breakifnot {length}
		if {shared->shared_history_index}
			assign {shared->shared_history_index - 1}, {shared->shared_history_index}
		endif
		devirt_call vector, ref_element, {shared->shared_history, shared->shared_history_index}, {string}
		func_call sys_mem, copy, {&string->string_data, &shared->shared_buffer, string->string_length}, \
									{_, shared->shared_bufp}
		func_call stream, create, {string, 0, &string->string_data, string->string_length}, {stream}
		local_call pipe_output, {shared, stream}, {r0, r1}
	elseif {char == 130}
		;cursor down
		devirt_call vector, get_length, {shared->shared_history}, {length}
		assign {shared->shared_history_index + 1}, {shared->shared_history_index}
		if {shared->shared_history_index > length}
			assign {length}, {shared->shared_history_index}
		endif
		if {shared->shared_history_index == length}
			func_call string, create_from_cstr, {""}, {string}
		else
			devirt_call vector, ref_element, {shared->shared_history, shared->shared_history_index}, {string}
		endif
		func_call sys_mem, copy, {&string->string_data, &shared->shared_buffer, string->string_length}, \
									{_, shared->shared_bufp}
		func_call stream, create, {string, 0, &string->string_data, string->string_length}, {stream}
		local_call pipe_output, {shared, stream}, {r0, r1}
	elseif {char == 27}
		;esc
		func_call master, get_state, {shared->shared_master}, {state}
		if {state == stream_mail_state_started}
			;feed active pipe, then EOF
			func_call master, get_input, {shared->shared_master}, {stream}
			func_call stream, write, {stream, &shared->shared_buffer, length}
			virt_call stream, write_flush, {stream}
			assign {&shared->shared_buffer}, {shared->shared_bufp}

			;send stopping
			func_call stream_msg_out, set_state, {stream, stream_mail_state_stopping}
			virt_call stream, write_next, {stream}
			virt_call stream, write_flush, {stream}
		endif
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
		func_call flow, get_first, {shared->shared_panel}, {label}
		func_call label, add_back, {label, shared->shared_panel}
		virt_call flow, layout, {shared->shared_panel}
		func_call string, create_from_cstr, {">"}, {string}
		func_call label, set_text, {label, string}
		func_call flow, dirty_all, {shared->shared_panel}
	elseif {char == 128}
		;backspace
		func_call flow, get_last, {shared->shared_panel}, {label}
		func_call label, get_text, {label}, {line_string}
		devirt_call string, get_length, {line_string}, {length}
		if {length > 1}
			assign {length - 1}, {length}
		endif
		func_call string, create_from_buffer, {&line_string->string_data, length}, {new_line_string}
		func_call string, deref, {line_string}
		func_call label, set_text, {label, new_line_string}
		func_call label, dirty, {label}
	elseif {char == 129 || char == 130}
		;cursor up/down
		func_call flow, get_last, {shared->shared_panel}, {label}
		func_call string, create_from_cstr, {">"}, {string}
		func_call label, set_text, {label, string}
		func_call label, dirty, {label}
	elseif {char >= 32 && char < 127}
		;append char
		func_call flow, get_last, {shared->shared_panel}, {label}
		func_call string, create_from_cstr, {&char}, {string}
		func_call label, get_text, {label}, {line_string}
		func_call string, append, {line_string, string}, {new_line_string}
		func_call string, deref, {line_string}
		func_call string, deref, {string}
		func_call label, set_text, {label, new_line_string}
		func_call label, dirty, {label}
	endif
	pop_scope
	return

def_func_end
