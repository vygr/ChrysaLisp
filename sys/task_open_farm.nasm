%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'

	fn_function sys/task_open_farm
		;inputs
		;r0 = name string object
		;r1 = number to spawn
		;outputs
		;r0 = array of mailbox id's
		;trashes
		;all but r4

		ptr name
		ulong length
		ptr ids
		ptr msg
		ulong cpu
		ulong index
		struct mailbox, ml_mailbox

		;save task info
		push_scope
		retire {r0, r1}, {name, length}

		;create output array
		static_call sys_mem, alloc, {length * mailbox_id_size}, {ids, _}

		;init temp mailbox
		static_call sys_mail, mailbox, {&mailbox}

		;start all tasks in parallel
		static_call sys_cpu, id, {}, {cpu}
		assign {0}, {index}
		loop_while {index != length}
			static_call sys_mail, alloc, {}, {msg}
			assign {name->string_length + 1 + kn_data_task_child_size}, {msg->ml_msg_length}
			assign {0}, {msg->ml_msg_dest.mb_mbox}
			assign {cpu}, {msg->ml_msg_dest.mb_cpu}
			assign {&mailbox}, {msg->kn_data_kernel_reply.mb_mbox}
			assign {cpu}, {msg->kn_data_kernel_reply.mb_cpu}
			assign {kn_call_task_child}, {msg->kn_data_kernel_function}
			assign {&ids[index * mailbox_id_size]}, {msg->kn_data_kernel_user}
			static_call sys_mem, copy, {&name->string_data, &msg->kn_data_task_child_pathname, \
										name->string_length + 1}, {_, _}
			static_call sys_mail, send, {msg}
			assign {index + 1}, {index}
		loop_end

		;wait for replys
		assign {0}, {index}
		loop_while {index != length}
			static_call sys_mail, read, {&mailbox}, {msg}
			assign {msg->kn_data_task_child_reply_mailboxid.mb_mbox}, {msg->kn_data_kernel_user->mb_mbox}
			assign {msg->kn_data_task_child_reply_mailboxid.mb_cpu}, {msg->kn_data_kernel_user->mb_cpu}
			static_call sys_mem, free, {msg}
			assign {index + 1}, {index}
		loop_end

		;return ids array
		eval {ids}, {r0}
		pop_scope
		vp_ret

	fn_function_end
