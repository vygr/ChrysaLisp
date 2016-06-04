%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'class/class_string.inc'

	fn_function sys/task_open_device
		;inputs
		;r0 = name string object
		;r1 = cpu target
		;outputs
		;r0 = mailbox id
		;r1 = mailbox id
		;trashes
		;all but r4

		ptr name
		ulong cpu
		ptr msg
		struct id, mailbox_id
		struct mailbox, ml_mailbox

		;save task info
		push_scope
		retire {r0, r1}, {name, cpu}

		;init temp mailbox
		static_call sys_mail, mailbox, {&mailbox}

		;start task
		static_call sys_mail, alloc, {}, {msg}
		assign {name->string_length + 1 + kn_data_task_open_size}, {msg->ml_msg_length}
		assign {0}, {msg->ml_msg_dest.mb_mbox}
		assign {cpu}, {msg->ml_msg_dest.mb_cpu}
		assign {&mailbox}, {msg->kn_data_kernel_reply.mb_mbox}
		static_call sys_cpu, id, {}, {msg->kn_data_kernel_reply.mb_cpu}
		assign {kn_call_task_open}, {msg->kn_data_kernel_function}
		static_call sys_mem, copy, {&name->string_data, &msg->kn_data_task_open_pathname, name->string_length + 1}, {_, _}
		static_call sys_mail, send, {msg}

		;wait for reply
		static_call sys_mail, read, {&mailbox}, {msg}
		assign {msg->kn_data_task_open_reply_mailboxid.mb_mbox}, {id.mb_mbox}
		assign {msg->kn_data_task_open_reply_mailboxid.mb_cpu}, {id.mb_cpu}
		static_call sys_mem, free, {msg}

		;return ids array
		eval {id.mb_mbox, id.mb_cpu}, {r0, r1}
		pop_scope
		vp_ret

	fn_function_end
