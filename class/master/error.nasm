%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_slave.inc'

	fn_function class/master/error
		;inputs
		;r0 = master object
		;r1 = buffer
		;r2 = buffer length
		;outputs
		;r0 = master object
		;r1 = amount read
		;trashes
		;all but r0, r4

		ptr inst
		ptr buffer
		ptr msg
		ulong length

		push_scope
		retire {r0, r1, r2}, {inst, buffer, length}

		static_call sys_mail, read, {&inst->master_error_mailbox}, {msg}
		assign {msg->msg_length - slave_mail_stream_size}, {length}
		static_call sys_mem, copy, {&msg->slave_mail_stream_data, buffer, length}, {_, _}
		static_call sys_mem, free, {msg}

		eval {inst, length}, {r0, r1}
		pop_scope
		vp_ret

	fn_function_end
