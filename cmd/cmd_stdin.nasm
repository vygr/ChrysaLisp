%include 'inc/func.inc'
%include 'cmd/cmd.inc'

	fn_function cmd/cmd_stdin
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
		ptr buffer_end
		ptr msg
		ulong length

		push_scope
		retire {r0, r1, r2}, {pipe, buffer, length}
		assign {0}, {msg}
		loop_start
			static_call cmd, next_msg, {&pipe->cmd_pipe_stdin_list, msg, pipe->cmd_pipe_stdin_seqnum}, {msg}
			breakif {msg != 0}
			static_call sys_mail, mymail, {}, {msg}
		loop_end
		assign {pipe->cmd_pipe_stdin_seqnum + 1}, {pipe->cmd_pipe_stdin_seqnum}
		assign {msg->ml_msg_length - cmd_mail_stream_size}, {length}
		static_call sys_mem, copy, {&msg->cmd_mail_stream_string, buffer, length}, {_, _}
		static_call sys_mem, free, {msg}
		eval {length}, {r0}
		pop_scope
		vp_ret

	fn_function_end
