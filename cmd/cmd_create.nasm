%include 'inc/func.inc'
%include 'cmd/cmd.inc'
%include 'class/class_string.inc'

	fn_function cmd/cmd_create
		;inputs
		;r0 = pipe
		;r1 = buffer
		;r2 = length
		;outputs
		;r0 = 0 if failed, else succsess
		;trashes
		;all but r4

		ptr pipe
		ptr buffer
		ulong length
		ptr msg
		ptr string
		pubyte charp

		push_scope
		retire {r0, r1, r2}, {pipe, buffer, length}

		;launch pipe
		assign {buffer + length}, {charp}
		assign {0}, {*charp}
		static_call string, create_from_cstr, {buffer}, {string}
		static_call sys_task, open_child, {string}, {pipe->cmd_master_input_mailbox_id.mb_mbox, pipe->cmd_master_input_mailbox_id.mb_cpu}
		static_call string, deref, {string}
		if {pipe->cmd_master_input_mailbox_id.mb_mbox != 0}
			;send args
			static_call sys_mail, alloc, {}, {msg}
			assign {cmd_mail_init_size + length}, {msg->ml_msg_length}
			assign {pipe->cmd_master_input_mailbox_id.mb_mbox}, {msg->ml_msg_dest.mb_mbox}
			assign {pipe->cmd_master_input_mailbox_id.mb_cpu}, {msg->ml_msg_dest.mb_cpu}
			assign {&pipe->cmd_master_output_mailbox}, {msg->cmd_mail_init_stdout_id.mb_mbox}
			static_call sys_cpu, id, {}, {msg->cmd_mail_init_stdout_id.mb_cpu}
			assign {&pipe->cmd_master_error_mailbox}, {msg->cmd_mail_init_stderr_id.mb_mbox}
			static_call sys_cpu, id, {}, {msg->cmd_mail_init_stderr_id.mb_cpu}
			static_call sys_mem, copy, {&buffer, &msg->cmd_mail_init_args, length}, {_, _}
			static_call sys_mail, send, {msg}

			;wait for acks
			static_call sys_mail, read, {&pipe->cmd_master_output_mailbox}, {msg}
			static_call sys_mem, free, {msg}

			;init seqnums
			assign {0, 0}, {pipe->cmd_master_input_seqnum, pipe->cmd_master_output_seqnum}

			;no error
			eval {1}, {r0}
		else
			;error
			eval {0}, {r0}
		endif
		pop_scope
		vp_ret

	fn_function_end
