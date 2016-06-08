%include 'inc/func.inc'
%include 'cmd/cmd.inc'

	fn_function cmd/cmd_error
		;inputs
		;r0 = pipe master object
		;r1 = buffer
		;r2 = buffer length
		;outputs
		;r0 = amount read
		;trashes
		;all but r4

		ptr pipe
		ptr buffer
		ptr msg
		ulong length

		push_scope
		retire {r0, r1, r2}, {pipe, buffer, length}
		static_call sys_mail, read, {&pipe->cmd_master_error_mailbox}, {msg}
		assign {msg->msg_length - cmd_mail_stream_size}, {length}
		static_call sys_mem, copy, {&msg->cmd_mail_stream_data, buffer, length}, {_, _}
		static_call sys_mem, free, {msg}
		eval {length}, {r0}
		pop_scope
		vp_ret

	fn_function_end
