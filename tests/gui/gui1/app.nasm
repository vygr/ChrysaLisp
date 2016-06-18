%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_grid.inc'
%include 'class/class_button.inc'
%include 'class/class_progress.inc'
%include 'class/class_string.inc'
%include 'tests/gui/gui1/app.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/app

		def_structure sel
			ptr sel_select1
			ptr sel_select2
		def_structure_end

		struct select, sel
		ptr window
		ptr window_panel
		ptr panel
		ptr left_panel
		ptr right_panel
		uint cpu_total
		uint cpu_count
		pptr task_progress
		ulong value
		ulong max_tasks
		ulong max_memory

		ptr msg
		ulong mailbox
		pulong task_mailboxes
		struct task_mailbox, mailbox

		ptr string
		ptr progress
		int width
		int height
		ulong owner

		;init app vars
		push_scope
		assign {0, mem_block_max_size * 32}, {max_tasks, max_memory}

		;create my window
		static_call window, create, {}, {window}
		static_call window, get_panel, {window}, {window_panel}
		static_call string, create_from_cstr, {"Network Task Monitor"}, {string}
		static_call window, set_title, {window, string}
		static_call string, create_from_cstr, {"Status Text"}, {string}
		static_call window, set_status, {window, string}

		;add my panels
		static_call grid, create, {}, {panel}
		static_call grid, set_grid, {panel, 2, 1}
		static_call grid, add_back, {panel, window_panel}
		static_call flow, create, {}, {left_panel}
		static_call flow, set_flow_flags, {left_panel, flow_flag_down | flow_flag_fillw}
		static_call flow, add_back, {left_panel, panel}
		static_call flow, create, {}, {right_panel}
		static_call flow, set_flow_flags, {right_panel, flow_flag_down | flow_flag_fillw}
		static_call flow, add_back, {right_panel, panel}

		;allocate array for progress bars
		static_call sys_cpu, total, {}, {cpu_total}
		static_call sys_mem, alloc, {cpu_total * ptr_size * 2}, {task_progress, _}

		;add num cpus progress bars to my app panel
		assign {0}, {cpu_count}
		loop_start
			static_call progress, create, {}, {progress}
			static_call progress, set_max, {progress, 1}
			static_call progress, set_color, {progress, 0xff00ff00}
			static_call progress, add_back, {progress, left_panel}
			assign {progress}, {task_progress[cpu_count * ptr_size * 2]}
			static_call progress, create, {}, {progress}
			static_call progress, set_max, {progress, 1}
			static_call progress, set_color, {progress, 0xffff0000}
			static_call progress, add_back, {progress, right_panel}
			assign {progress}, {task_progress[cpu_count * ptr_size * 2 + ptr_size]}
			assign {cpu_count + 1}, {cpu_count}
		loop_until {cpu_count == cpu_total}

		;set to pref size
		method_call window, pref_size, {window}, {width, height}
		static_call window, change, {window, 32, 32, width, height}

		;set owner
		static_call sys_task, tcb, {}, {owner}
		static_call window, set_owner, {window, owner}

		;add to screen and dirty
		static_call gui_gui, add, {window}
		static_call window, dirty_all, {window}

		;open global farm
		static_call string, create_from_cstr, {"tests/gui/gui1/child"}, {string}
		static_call sys_task, open_global, {string, cpu_total}, {task_mailboxes}
		static_call string, deref, {string}

		;init task mailbox
		static_call sys_mail, init_mailbox, {&task_mailbox}

		;set up mailbox select array
		static_call sys_task, mailbox, {}, {select.sel_select1, _}
		assign {&task_mailbox}, {select.sel_select2}

		;app event loop
		loop_start
			;new round of samples ?
			if {cpu_count ==  cpu_total}
				;send out sample commands
				loop_start
					assign {cpu_count - 1}, {cpu_count}
					static_call sys_mail, alloc, {}, {msg}
					assign {1}, {msg->sample_msg_command}
					assign {sample_msg_size}, {msg->msg_length}
					assign {cpu_count * ptr_size * 2}, {msg->sample_msg_progress}
					assign {task_mailboxes[cpu_count * id_size].id_mbox}, {msg->msg_dest.id_mbox}
					assign {task_mailboxes[cpu_count * id_size].id_cpu}, {msg->msg_dest.id_cpu}
					assign {select.sel_select2}, {msg->sample_msg_reply_id.id_mbox}
					static_call sys_cpu, id, {}, {msg->sample_msg_reply_id.id_cpu}
					static_call sys_mail, send, {msg}
				loop_until {!cpu_count}
			endif

			;select on multiple mailboxes
			static_call sys_mail, select, {&select, sel_size >> 3}, {mailbox}
			static_call sys_mail, read, {mailbox}, {msg}

			;which mailbox had mail ?
			if {mailbox == select.sel_select1}
				;dispatch event to view
				method_call view, event, {msg->ev_msg_view, msg}
			else
				;update progress bars
				assign {msg->sample_msg_task_count}, {value}
				if {value > max_tasks}
					assign {value}, {max_tasks}
				endif
				assign {task_progress[msg->sample_msg_progress]}, {progress}
				static_call progress, set_max, {progress, max_tasks}
				static_call progress, set_val, {progress, value}
				static_call progress, dirty, {progress}

				assign {msg->sample_msg_mem_used}, {value}
;				if {value > max_memory}
;					assign {value}, {max_memory}
;				endif
				assign {task_progress[msg->sample_msg_progress + ptr_size]}, {progress}
				static_call progress, set_max, {progress, max_memory}
				static_call progress, set_val, {progress, value}
				static_call progress, dirty, {progress}

				;count up replies
				assign {cpu_count + 1}, {cpu_count}
			endif

			;free event message
			static_call sys_mem, free, {msg}

			;be friendly
			static_call sys_task, yield
		loop_end

		;wait for outstanding replys
		loop_while {cpu_count != cpu_total}
			static_call sys_mail, read, {select.sel_select2}, {msg}
			static_call sys_mem, free, {msg}
			assign {cpu_count + 1}, {cpu_count}
		loop_end

		;send out exit commands
		loop_start
			assign {cpu_count - 1}, {cpu_count}
			static_call sys_mail, alloc, {}, {msg}
			assign {0}, {msg->sample_msg_command}
			assign {sample_msg_size}, {msg->msg_length}
			assign {task_mailboxes[cpu_count * id_size].id_mbox}, {msg->msg_dest.id_mbox}
			assign {task_mailboxes[cpu_count * id_size].id_cpu}, {msg->msg_dest.id_cpu}
			static_call sys_mail, send, {msg}
		loop_until {!cpu_count}

		;free arrays
		static_call sys_mem, free, {task_mailboxes}
		static_call sys_mem, free, {task_progress}

		;deref window
		static_call window, deref, {window}

		pop_scope
		return

	fn_function_end
