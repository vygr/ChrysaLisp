%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/string.inc'
%include 'inc/list.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_label.inc'
%include 'class/class_string.inc'
%include 'cmd/cmd.inc'

	fn_function cmd/cmd

		buffer_size equ 120

		def_structure shared
			struct shared_pipe, cmd_master
			ptr shared_panel
			pubyte shared_bufp
			struct shared_buffer, buffer
			ubyte shared_mode
		def_structure_end

		def_structure sel
			ptr sel_event
			ptr sel_stdout
			ptr sel_stderr
		def_structure_end

		struct myapp, obj
		struct shared, shared
		struct sel, sel
		struct buffer, buffer
		ptr msg
		ptr window
		ptr window_panel
		ptr label
		pubyte next
		ptr string
		ulong owner
		ptr mailbox
		ulong length
		int width
		int height
		int char

		;init app vars
		push_scope
		slot_function class, obj
		static_call obj, init, {&myapp, @_function_}, {_}
		static_call cmd, master, {&shared.shared_pipe}
		assign {0}, {shared.shared_mode}

		;create my window
		static_call window, create, {}, {window}
		static_call window, get_panel, {window}, {window_panel}
		static_call string, create_from_cstr, {"Terminal"}, {string}
		static_call window, set_title, {window, string}
		static_call string, create_from_cstr, {"Ready"}, {string}
		static_call window, set_status, {window, string}

		;add my app panel
		static_call flow, create, {}, {shared.shared_panel}
		static_call flow, set_flow_flags, {shared.shared_panel, flow_flag_down | flow_flag_fillw}
		static_call flow, add_back, {shared.shared_panel, window_panel}

		;add term lines to my app panel
		assign {$line_list}, {next}
		loop_start
			breakifnot {*next}

			static_call label, create, {}, {label}
			static_call string, create_from_cstr, {next}, {string}
			static_call label, set_text, {label, string}
			static_call label, set_color, {label, 0xff000000}
			static_call label, set_text_color, {label, 0xff00ff00}
			static_call label, set_font, {label, "fonts/OpenSans-Regular.ttf", 16}
			static_call label, add_back, {label, shared.shared_panel}

			static_call sys_string, length, {next}, {length}
			assign {next + length + 1}, {next}
		loop_end

		;set to pref size
		method_call window, pref_size, {window}, {width, height}
		static_call window, change, {window, 0, 0, 640, height}

		;set window owner
		static_call sys_task, tcb, {}, {owner}
		static_call window, set_owner, {window, owner}

		;add to screen and dirty
		static_call gui_gui, add, {window}
		static_call window, dirty_all, {window}

		;set up term buffer
		assign {&shared.shared_buffer}, {shared.shared_bufp}

		;set up mailbox select array
		static_call sys_task, mailbox, {}, {sel.sel_event, _}
		assign {&shared.shared_pipe.cmd_master_output_mailbox}, {sel.sel_stdout}
		assign {&shared.shared_pipe.cmd_master_error_mailbox}, {sel.sel_stderr}

		;app event loop
		loop_start
			;select on multiple mailboxes
			static_call sys_mail, select, {&sel, sel_size >> 3}, {mailbox}

			;which mailbox has mail ?
			if {mailbox == sel.sel_event}
				;dispatch event to view and terminal
				static_call sys_mail, read, {mailbox}, {msg}
				method_call view, event, {msg->ev_msg_view, msg}
				if {msg->ev_msg_type == ev_type_key && msg->ev_msg_keycode > 0}
					local_call terminal_input, {&shared, msg->ev_msg_key}, {r0, r1}
				endif
				static_call sys_mem, free, {msg}
			elseif {mailbox == sel.sel_stderr}
				;output from stderr
				static_call cmd, error, {&shared.shared_pipe, &buffer, buffer_size}, {length}
				local_call pipe_output, {&shared, buffer, length}, {r0, r1, r2}
			else
				;output from stdout
				static_call cmd, output, {&shared.shared_pipe, &buffer, buffer_size}, {length}
				local_call pipe_output, {&shared, buffer, length}, {r0, r1, r2}
			endif
		loop_end

		;deref window
		static_call window, deref, {window}
		method_call obj, deinit, {&myapp}
		pop_scope
		vp_ret

	pipe_output:
		;inputs
		;r0 = shared
		;r1 = buffer
		;r2 = length

		ptr shared
		ptr buffer
		ulong length
		pubyte charp

		push_scope
		retire {r0, r1, r2}, {shared, buffer, length}
		assign {buffer}, {charp}
		loop_while {charp != (buffer + length)}
			local_call terminal_output, {shared, *charp}, {r0, r1}
			assign {charp + 1}, {charp}
		loop_end
		pop_scope
		vp_ret

	terminal_input:
		;inputs
		;r0 = shared
		;r1 = char input

		ptr shared
		ptr msg
		ubyte char

		push_scope
		retire {r0, r1}, {shared, char}

		;echo char to terminal
		local_call terminal_output, {shared, char}, {r0, r1}

		;buffer char
		assign {char}, {*shared->shared_bufp}

		;send line ?
		if {char == 10 || char == 13}
			;what mode ?
			if {shared->shared_mode == 0}
				;create new pipe
				static_call cmd, create, {&shared->shared_pipe, &shared->shared_buffer, \
				 			shared->shared_bufp - &shared->shared_buffer}, {shared->shared_mode}
			else
				;feed active pipe
				static_call cmd, input, {&shared->shared_pipe, &shared->shared_buffer, \
							shared->shared_bufp + 1 - &shared->shared_buffer}
			endif

			;reset char pointer
			assign {&shared->shared_buffer}, {shared->shared_bufp}
		else
			;next char
			assign {shared->shared_bufp + 1}, {shared->shared_bufp}
		endif
		pop_scope
		vp_ret

	terminal_output:
		;inputs
		;r0 = shared
		;r1 = char output

		ptr shared
		ptr label
		ptr string
		ptr line_string
		ptr new_line_string
		ulong char

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
		else
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
		vp_ret

	line_list:
		%rep 27
			db '>', 0
		%endrep
		db '>Terminal', 0
		db '>(C) C.A.Hinsley 2016', 0
		db '>', 0
		db 0

	fn_function_end
