%include 'inc/func.inc'
%include 'class/class_slave.inc'

	fn_function class/slave/stdin
		;inputs
		;r0 = slave object
		;r1 = buffer
		;r2 = buffer length
		;outputs
		;r0 = slave object
		;r1 = amount read
		;trashes
		;all but r0, r4

		ptr inst
		ptr buffer
		ulong length
		ptr msg

		push_scope
		retire {r0, r1, r2}, {inst, buffer, length}

		assign {0}, {msg}
		loop_start
			static_call slave, next_seq, {&inst->slave_stdin_list, msg, inst->slave_stdin_seqnum}, {msg}
			breakif {msg}
			static_call sys_mail, mymail, {}, {msg}
		loop_end
		assign {inst->slave_stdin_seqnum + 1}, {inst->slave_stdin_seqnum}
		assign {msg->msg_length - slave_mail_stream_size}, {length}
		static_call sys_mem, copy, {&msg->slave_mail_stream_data, buffer, length}, {_, _}
		static_call sys_mem, free, {msg}

		eval {inst, length}, {r0, r1}
		pop_scope
		vp_ret

	fn_function_end
