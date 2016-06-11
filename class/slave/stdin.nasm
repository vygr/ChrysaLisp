%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_slave.inc'

	fn_function class/slave/stdin
		;inputs
		;r0 = slave object
		;outputs
		;r0 = slave object
		;r1 = 0 if EOF, else stream object
		;trashes
		;all but r0, r4

		ptr inst
		ptr stream
		ptr msg
		ulong length

		push_scope
		retire {r0}, {inst}

		assign {0}, {msg}
		loop_start
			static_call slave, next_seq, {&inst->slave_stdin_list, msg, inst->slave_stdin_seqnum}, {msg}
			breakif {msg}
			static_call sys_mail, mymail, {}, {msg}
		loop_end
		assign {inst->slave_stdin_seqnum + 1}, {inst->slave_stdin_seqnum}

		static_call stream, create, {0, msg, &msg->slave_mail_stream_data, msg->msg_length - slave_mail_stream_size}, {stream}
		static_call stream, available, {stream}, {length}
		if {!length}
			static_call stream, deref, {stream}
			assign {0}, {stream}
		endif

		eval {inst, stream}, {r0, r1}
		pop_scope
		return

	fn_function_end
