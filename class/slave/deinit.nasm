%include 'inc/func.inc'
%include 'class/class_slave.inc'
%include 'class/class_stream_msg_out.inc'
%include 'class/class_vector.inc'

	fn_function class/slave/deinit
		;inputs
		;r0 = slave object
		;trashes
		;all but r0, r4

		ptr inst
		ptr msg

		push_scope
		retire {r0}, {inst}

		;send normal EOF
		method_call stream_msg_out, write_next, {inst->slave_stdout}
		method_call stream_msg_out, write_next, {inst->slave_stdout}

		;wait for master EOF
		loop_start
			assign {0}, {msg}
			loop_start
				static_call stream_msg_out, next_seq, {&inst->slave_stdin_list, msg, inst->slave_stdin_seqnum}, {msg}
				breakif {msg}
				static_call sys_mail, mymail, {}, {msg}
			loop_end
			assign {inst->slave_stdin_seqnum + 1}, {inst->slave_stdin_seqnum}
			breakif {msg->stream_mail_state == stream_mail_state_stopped}
			static_call sys_mem, free, {msg}
		loop_end
		static_call sys_mem, free, {msg}

		;send master EOF
		assign {stream_mail_state_stopped}, {inst->slave_stdout->stream_msg_out_state}
		method_call stream_msg_out, write_flush, {inst->slave_stdout}

		;free stdout and stderr
		static_call stream_msg_out, deref, {inst->slave_stdout}
		static_call stream_msg_out, deref, {inst->slave_stderr}

		;free args
		static_call vector, deref, {inst->slave_args}

		eval {inst}, {r0}
		pop_scope

		;deinit parent
		p_jmp slave, deinit, {r0}

	fn_function_end
