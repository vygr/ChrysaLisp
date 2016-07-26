%include 'inc/func.inc'
%include 'class/class_slave.inc'
%include 'class/class_stream_msg_out.inc'
%include 'class/class_stream_msg_in.inc'
%include 'class/class_vector.inc'

	fn_function class/slave/deinit
		;inputs
		;r0 = slave object
		;trashes
		;all but r0, r4

		ptr inst, msg

		push_scope
		retire {r0}, {inst}

		;flush remaining
		method_call stream_msg_out, write_flush, {inst->slave_stderr}
		method_call stream_msg_out, write_flush, {inst->slave_stdout}

		;send stopping on stdout
		static_call stream_msg_out, set_state, {inst->slave_stdout, stream_mail_state_stopping}
		method_call stream_msg_out, write_next, {inst->slave_stdout}
		method_call stream_msg_out, write_flush, {inst->slave_stdout}

		;wait for stopped
		loop_start
			method_call stream_msg_in, read_next, {inst->slave_stdin}, {_}
		loop_until {inst->slave_stdin->stream_msg_in_state == stream_mail_state_stopped}

		;send stopped on stdout and stderr
		static_call stream_msg_out, set_state, {inst->slave_stdout, stream_mail_state_stopped}
		static_call stream_msg_out, set_state, {inst->slave_stderr, stream_mail_state_stopped}
		method_call stream_msg_out, write_next, {inst->slave_stdout}
		method_call stream_msg_out, write_flush, {inst->slave_stdout}
		method_call stream_msg_out, write_next, {inst->slave_stderr}
		method_call stream_msg_out, write_flush, {inst->slave_stderr}

		;free stdin, stdout and stderr
		static_call stream_msg_in, deref, {inst->slave_stdin}
		static_call stream_msg_out, deref, {inst->slave_stdout}
		static_call stream_msg_out, deref, {inst->slave_stderr}

		;free args
		static_call vector, deref, {inst->slave_args}

		eval {inst}, {r0}
		pop_scope

		;deinit parent
		p_jmp slave, deinit, {r0}

	fn_function_end
