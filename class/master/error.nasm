%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_slave.inc'
%include 'class/class_stream.inc'

	fn_function class/master/error
		;inputs
		;r0 = master object
		;outputs
		;r0 = master object
		;r1 = 0 if EOF, else stream
		;trashes
		;all but r0, r4

		ptr inst
		ptr stream
		ptr msg
		ulong length

		push_scope
		retire {r0}, {inst}

		static_call sys_mail, read, {&inst->master_error_mailbox}, {msg}
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
