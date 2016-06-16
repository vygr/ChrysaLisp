%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'
%include 'class/class_slave.inc'

	fn_function class/slave/init
		;inputs
		;r0 = slave object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok
		;trashes
		;all but r0, r4

		const space_char, ' '

		ptr inst
		ptr vtable
		ptr stream
		ptr msg
		ulong error

		;read init args
		push_scope
		retire {r0, r1}, {inst, vtable}

		;init parent
		super_call slave, init, {inst, vtable}, {error}
		if {error != 0}
			;init myself
			static_call sys_mail, mymail, {}, {msg}
			if {msg->msg_length != msg_header_size}
				static_call stream_msg_out, create, {msg->slave_mail_init_stdout_id.id_mbox, \
													msg->slave_mail_init_stdout_id.id_cpu}, {inst->slave_stdout}
				static_call stream_msg_out, create, {msg->slave_mail_init_stderr_id.id_mbox, \
													msg->slave_mail_init_stderr_id.id_cpu}, {inst->slave_stderr}
				static_call stream, create, {0, 0, &msg->slave_mail_init_args, \
											msg->msg_length - slave_mail_init_size}, {stream}
				static_call stream, split, {stream, space_char}, {inst->slave_args}
				static_call stream, deref, {stream}

				;send back ack
				assign {msg->slave_mail_init_ack_id.id_mbox}, {msg->msg_dest.id_mbox}
				assign {msg->slave_mail_init_ack_id.id_cpu}, {msg->msg_dest.id_cpu}
				assign {msg_header_size}, {msg->msg_length}
				static_call sys_mail, send, {msg}

				;init order lists
				static_call sys_list, init, {&inst->slave_stdin_list}
			else
				;abort
				static_call sys_mail, send, {msg}
				assign {0}, {error}
			endif
		endif

		eval {inst, error}, {r0, r1}
		pop_scope
		return

	fn_function_end
