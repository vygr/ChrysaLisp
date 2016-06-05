%include 'inc/func.inc'
%include 'cmd/cmd.inc'

	fn_function cmd/cmd_output
		;inputs
		;r0 = pipe
		;r1 = buffer
		;r2 = buffer length
		;outputs
		;r0 = amount read
		;trashes
		;all but r4

		ptr pipe
		ptr buffer
		ulong length
		ptr msg

		push_scope
		retire {r0, r1, r2}, {pipe, buffer, length}
		assign {0}, {msg}
		loop_start
			static_call cmd, next_msg, {&pipe->cmd_master_output_list, msg, pipe->cmd_master_output_seqnum}, {msg}
			breakif {msg != 0}
			static_call sys_mail, read, {&pipe->cmd_master_output_mailbox}, {msg}
		loop_end
		assign {pipe->cmd_master_output_seqnum + 1}, {pipe->cmd_master_output_seqnum}
		assign {msg->msg_length - cmd_mail_stream_size}, {length}
		static_call sys_mem, copy, {&msg->cmd_mail_stream_data, buffer, length}, {_, _}
		static_call sys_mem, free, {msg}
		eval {length}, {r0}
		pop_scope
		vp_ret

	fn_function_end
