%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'

	fn_function sys/task_open_pipe
		;inputs
		;r0 = vector of strings
		;outputs
		;r0 = array of mailbox id's
		;trashes
		;all but r4

		ptr tasks
		ptr ids
		ptr msg
		ptr name
		ulong cpu
		ulong index
		ulong length
		struct mailbox, ml_mailbox

		;save task info
		push_scope
		retire {r0}, {tasks}

		;create output array
		static_call vector, get_length, {tasks}, {length}
		static_call sys_mem, alloc, {length * mailbox_id_size}, {ids, _}

		;init temp mailbox
		static_call sys_mail, mailbox, {&mailbox}

		;start all tasks, starting on kernel of this chip
		static_call sys_cpu, id, {}, {cpu}
		assign {0}, {index}
		loop_while {index != length}
			static_call sys_mail, alloc, {}, {msg}
			assign {(tasks->vector_array)[index * ptr_size]}, {name}
			assign {name->string_length + 1 + kn_data_task_child_size}, {msg->ml_msg_length}
			assign {0}, {msg->ml_msg_dest.mb_mbox}
			assign {cpu}, {msg->ml_msg_dest.mb_cpu}
			assign {&mailbox}, {msg->kn_data_kernel_reply.mb_mbox}
			static_call sys_cpu, id, {}, {msg->kn_data_kernel_reply.mb_cpu}
			assign {kn_call_task_child}, {msg->kn_data_kernel_function}
			static_call sys_mem, copy, {&name->string_data, &msg->kn_data_task_child_pathname, \
										name->string_length + 1}, {_, _}

			;send mail to kernel, wait for reply
			static_call sys_mail, send, {msg}
			static_call sys_mail, read, {&mailbox}, {msg}

			;save reply mailbox ID
			assign {msg->kn_data_task_child_reply_mailboxid.mb_cpu}, {cpu}
			assign {msg->kn_data_task_child_reply_mailboxid.mb_mbox}, {ids[index * mailbox_id_size].mb_mbox}
			assign {cpu}, {ids[index * mailbox_id_size].mb_cpu}
			static_call sys_mem, free, {msg}

			;next pipe worker
			assign {index + 1}, {index}
		loop_end

		;return ids array
		eval {ids}, {r0}
		pop_scope
		vp_ret

	fn_function_end
