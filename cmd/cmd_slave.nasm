%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'cmd/cmd.inc'

	fn_function cmd/cmd_slave
		;inputs
		;r0 = pipe struct pointer
		;trashes
		;all but r4

		ptr pipe
		ptr msg
		ulong length

		;read init args
		push_scope
		retire {r0}, {pipe}
		static_call sys_mail, mymail, {}, {msg}
		assign {msg->cmd_mail_init_stdout_id.id_mbox}, {pipe->cmd_slave_stdout_id.id_mbox}
		assign {msg->cmd_mail_init_stdout_id.id_cpu}, {pipe->cmd_slave_stdout_id.id_cpu}
		assign {msg->cmd_mail_init_stderr_id.id_mbox}, {pipe->cmd_slave_stderr_id.id_mbox}
		assign {msg->cmd_mail_init_stderr_id.id_cpu}, {pipe->cmd_slave_stderr_id.id_cpu}
		assign {msg->msg_length - cmd_mail_init_size}, {length}
		assign {length}, {pipe->cmd_slave_args_length}
		static_call sys_mem, copy, {&msg->cmd_mail_init_args, &pipe->cmd_slave_args, length}, {_, _}

		;send back ack
		assign {msg->cmd_mail_init_stdout_id.id_mbox}, {msg->msg_dest.id_mbox}
		assign {msg->cmd_mail_init_stdout_id.id_cpu}, {msg->msg_dest.id_cpu}
		assign {msg_data}, {msg->msg_length}
		static_call sys_mail, send, {msg}

		;init seqnums
		assign {0, 0}, {pipe->cmd_slave_stdout_seqnum, pipe->cmd_slave_stdin_seqnum}

		;init order lists
		static_call sys_list, init, {&pipe->cmd_slave_stdin_list}

		pop_scope
		vp_ret

	fn_function_end
