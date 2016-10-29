%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_grid.inc'
%include 'class/class_button.inc'
%include 'class/class_progress.inc'
%include 'class/class_string.inc'
%include 'apps/netmon/app.inc'

def_func apps/netmon/app

	def_struc sel
		ptr sel_select1
		ptr sel_select2
	def_struc_end

	struct select, sel
	ptr window, window_panel, panel, left_panel, right_panel
	pptr task_progress
	ulong value, max_tasks, max_memory, total_memory
	uint cpu_total, cpu_count

	ptr msg
	ulong mailbox
	pulong task_mailboxes
	struct task_mailbox, mailbox

	ptr string, progress
	int width, height
	ulong owner

	;init app vars
	push_scope
	assign {0}, {max_tasks}

	;create my window
	func_call window, create, {}, {window}
	func_call window, get_panel, {window}, {window_panel}
	func_call string, create_from_cstr, {"Network Monitor"}, {string}
	func_call window, set_title, {window, string}
	func_call string, create_from_cstr, {"Status Text"}, {string}
	func_call window, set_status, {window, string}

	;add my panels
	func_call grid, create, {}, {panel}
	func_call grid, set_grid, {panel, 2, 1}
	func_call grid, add_back, {panel, window_panel}
	func_call flow, create, {}, {left_panel}
	func_call flow, set_flow_flags, {left_panel, flow_flag_down | flow_flag_fillw}
	func_call flow, add_back, {left_panel, panel}
	func_call flow, create, {}, {right_panel}
	func_call flow, set_flow_flags, {right_panel, flow_flag_down | flow_flag_fillw}
	func_call flow, add_back, {right_panel, panel}

	;allocate array for progress bars
	func_call sys_cpu, total, {}, {cpu_total}
	func_call sys_mem, alloc, {cpu_total * ptr_size * 2}, {task_progress, _}

	;add num cpus progress bars to my app panel
	assign {0}, {cpu_count}
	loop_start
		func_call progress, create, {}, {progress}
		func_call progress, set_max, {progress, 1}
		func_call progress, set_color, {progress, 0xff00ff00}
		func_call progress, add_back, {progress, left_panel}
		assign {progress}, {task_progress[cpu_count * ptr_size * 2]}
		func_call progress, create, {}, {progress}
		func_call progress, set_max, {progress, 1}
		func_call progress, set_color, {progress, 0xffff0000}
		func_call progress, add_back, {progress, right_panel}
		assign {progress}, {task_progress[cpu_count * ptr_size * 2 + ptr_size]}
		assign {cpu_count + 1}, {cpu_count}
	loop_until {cpu_count == cpu_total}

	;set to pref size
	virt_call window, pref_size, {window}, {width, height}
	func_call window, change, {window, 32, 32, width, height}

	;set owner
	func_call sys_task, tcb, {}, {owner}
	func_call window, set_owner, {window, owner}

	;add to screen and dirty
	func_call gui_gui, add, {window}
	func_call window, dirty_all, {window}

	;open global farm
	func_call string, create_from_cstr, {"apps/netmon/child"}, {string}
	func_call sys_task, open_global, {string, cpu_total}, {task_mailboxes}
	func_call string, deref, {string}

	;init task mailbox
	func_call sys_mail, init_mailbox, {&task_mailbox}

	;set up mailbox select array
	func_call sys_task, mailbox, {}, {select.sel_select1, _}
	assign {&task_mailbox}, {select.sel_select2}

	;app event loop
	loop_start
		;new round of samples ?
		if {cpu_count ==  cpu_total}
			;set max_memory level
			assign {(total_memory * 3) / (cpu_total * 2) + 1}, {max_memory}
			assign {0}, {total_memory}

			;send out sample commands
			loop_start
				assign {cpu_count - 1}, {cpu_count}
				func_call sys_mail, alloc, {}, {msg}
				assign {1}, {msg->sample_msg_command}
				assign {sample_msg_size}, {msg->msg_length}
				assign {cpu_count * ptr_size * 2}, {msg->sample_msg_index}
				assign {task_mailboxes[cpu_count * id_size].id_mbox}, {msg->msg_dest.id_mbox}
				assign {task_mailboxes[cpu_count * id_size].id_cpu}, {msg->msg_dest.id_cpu}
				assign {select.sel_select2}, {msg->sample_msg_reply_id.id_mbox}
				func_call sys_cpu, id, {}, {msg->sample_msg_reply_id.id_cpu}
				func_call sys_mail, send, {msg}
			loop_untilnot {cpu_count}
		endif

		;select on multiple mailboxes
		func_call sys_mail, select, {&select, sel_size >> 3}, {mailbox}
		func_call sys_mail, read, {mailbox}, {msg}

		;which mailbox had mail ?
		if {mailbox == select.sel_select1}
			;dispatch event to view
			virt_call view, event, {msg->ev_msg_view, msg}
		else
			;update progress bars
			assign {msg->sample_msg_task_count}, {value}
			if {value > max_tasks}
				assign {value}, {max_tasks}
			endif
			assign {task_progress[msg->sample_msg_index]}, {progress}
			func_call progress, set_max, {progress, max_tasks}
			func_call progress, set_val, {progress, value}
			func_call progress, dirty, {progress}

			assign {msg->sample_msg_mem_used}, {value}
			assign {total_memory + value}, {total_memory}
			assign {task_progress[msg->sample_msg_index + ptr_size]}, {progress}
			func_call progress, set_max, {progress, max_memory}
			func_call progress, set_val, {progress, value}
			func_call progress, dirty, {progress}

			;count up replies
			assign {cpu_count + 1}, {cpu_count}
		endif

		;free event message
		func_call sys_mem, free, {msg}

		;be friendly
		func_call sys_task, yield
	loop_end

	;wait for outstanding replys
	loop_while {cpu_count != cpu_total}
		func_call sys_mail, read, {select.sel_select2}, {msg}
		func_call sys_mem, free, {msg}
		assign {cpu_count + 1}, {cpu_count}
	loop_end

	;send out exit commands
	loop_start
		assign {cpu_count - 1}, {cpu_count}
		func_call sys_mail, alloc, {}, {msg}
		assign {0}, {msg->sample_msg_command}
		assign {sample_msg_size}, {msg->msg_length}
		assign {task_mailboxes[cpu_count * id_size].id_mbox}, {msg->msg_dest.id_mbox}
		assign {task_mailboxes[cpu_count * id_size].id_cpu}, {msg->msg_dest.id_cpu}
		func_call sys_mail, send, {msg}
	loop_untilnot {cpu_count}

	;free arrays
	func_call sys_mem, free, {task_mailboxes}
	func_call sys_mem, free, {task_progress}

	;deref window
	func_call window, deref, {window}

	pop_scope
	return

def_func_end
