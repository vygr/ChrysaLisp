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

		def_structure term
			pubyte term_bufp
			ptr term_panel
			struct term_buf, buffer
		def_structure_end

		def_structure sel
			ptr sel_event
			ptr sel_stdout
			ptr sel_stderr
		def_structure_end

		struct myapp, obj
		struct terminal, term
		struct select, sel
		struct stdout_mailbox, ml_mailbox
		struct stderr_mailbox, ml_mailbox
		struct stdout_list, lh_list
		ulong stdin_seqnum
		ulong stdout_seqnum
		ptr msg
		ptr window
		ptr window_panel
		ptr label
		pubyte next
		ptr string
		ulong owner
		ulong mailbox
		pubyte charp
		int width
		int height
		int char
		ubyte length

		;init app vars
		push_scope
		slot_function class, obj
		static_call obj, init, {&myapp, @_function_}, {_}
		assign {0, 0}, {stdin_seqnum, stdout_seqnum}
		static_call sys_list, init, {&stdout_list}

		;create my window
		static_call window, create, {}, {window}
		static_call window, get_panel, {window}, {window_panel}
		static_call string, create_from_cstr, {"Terminal"}, {string}
		static_call window, set_title, {window, string}
		static_call string, create_from_cstr, {"Ready"}, {string}
		static_call window, set_status, {window, string}

		;add my app panel
		static_call flow, create, {}, {terminal.term_panel}
		static_call flow, set_flow_flags, {terminal.term_panel, flow_flag_down | flow_flag_fillw}
		static_call flow, add_back, {terminal.term_panel, window_panel}

		;add terminal lines to my app panel
		assign {$line_list}, {next}
		loop_start
			breakifnot {*next}

			static_call label, create, {}, {label}
			static_call string, create_from_cstr, {next}, {string}
			static_call label, set_text, {label, string}
			static_call label, set_color, {label, 0xff000000}
			static_call label, set_text_color, {label, 0xff00ff00}
			static_call label, set_font, {label, "fonts/OpenSans-Regular.ttf", 16}
			static_call label, add_back, {label, terminal.term_panel}

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

		;set up terminal buffer
		assign {&terminal.term_buf}, {terminal.term_bufp}

		;init stdout and stderr mailboxes
		static_call sys_mail, mailbox, {&stdout_mailbox}
		static_call sys_mail, mailbox, {&stderr_mailbox}

		;set up mailbox select array
		static_call sys_task, mailbox, {}, {select.sel_event, _}
		assign {&stdout_mailbox, &stderr_mailbox}, {select.sel_stdout, select.sel_stderr}

		;app event loop
		loop_start
			;select on multiple mailboxes
			static_call sys_mail, select, {&select, sel_size >> 3}, {mailbox}
			static_call sys_mail, read, {mailbox}, {msg}

			;which mailbox had mail ?
			if {mailbox == select.sel_event}
				;dispatch event to view
				method_call view, event, {msg->ev_data_view, msg}

				;if key event, then input to command
				if {msg->ev_data_type == ev_type_key && msg->ev_data_keycode > 0}
					local_call terminal_input, {&terminal, msg->ev_data_key}, {r0, r1}
				endif
				static_call sys_mem, free, {msg}
			elseif {mailbox == select.sel_stderr}
				;input from stderr
				assign {&msg->cmd_mail_string}, {charp}
				loop_while {*charp != 0}
					local_call terminal_output, {&terminal, *charp}, {r0, r1}
					assign {charp + 1}, {charp}
				loop_end
				static_call sys_mem, free, {msg}
			else
				;input from stdout
				loop_start
					static_call cmd, next_msg, {&stdout_list, msg, stdout_seqnum}, {msg}
					breakif {msg == 0}
					assign {stdout_seqnum + 1}, {stdout_seqnum}
					assign {&msg->cmd_mail_string}, {charp}
					loop_while {*charp != 0}
						local_call terminal_output, {&terminal, *charp}, {r0, r1}
						assign {charp + 1}, {charp}
					loop_end
					static_call sys_mem, free, {msg}
					assign {0}, {msg}
				loop_end
			endif
		loop_end

		;deref window
		static_call window, deref, {window}
		method_call obj, deinit, {&myapp}
		pop_scope
		vp_ret

	terminal_input:
		;inputs
		;r0 = terminal
		;r1 = char input

		ptr terminal
		ubyte char

		push_scope
		retire {r0, r1}, {terminal, char}
		local_call terminal_output, {terminal, char}, {r0, r1}
		if {char == 10 || char == 13}
			;send line to command pipeline

			;reset buffer
			assign {&terminal->term_buf}, {terminal->term_bufp}
		else
			;buffer char
			assign {char}, {*terminal->term_bufp}
			assign {terminal->term_bufp + 1}, {terminal->term_bufp}
		endif
		pop_scope
		vp_ret

	terminal_output:
		;inputs
		;r0 = terminal
		;r1 = char output

		ptr terminal
		ptr label
		ptr string
		ptr line_string
		ptr new_line_string
		ulong char

		push_scope
		retire {r0, r1}, {terminal, char}
		if {char == 10 || char == 13}
			;scroll lines
			static_call flow, get_first, {terminal->term_panel}, {label}
			static_call label, add_back, {label, terminal->term_panel}
			method_call flow, layout, {terminal->term_panel}
			static_call string, create_from_cstr, {">"}, {string}
			static_call label, set_text, {label, string}
			static_call flow, dirty_all, {terminal->term_panel}
		else
			;append char
			static_call flow, get_last, {terminal->term_panel}, {label}
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
