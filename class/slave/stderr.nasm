%include 'inc/func.inc'
%include 'class/class_slave.inc'
%include 'class/class_master.inc'

	fn_function class/slave/stderr
		;inputs
		;r0 = slave object
		;r1 = buffer
		;r2 = length
		;trashes
		;all but r0, r4

		ptr inst
		ptr buffer
		ulong length
		ptr msg

		push_scope
		retire {r0, r1, r2}, {inst, buffer, length}

		static_call sys_mail, alloc, {}, {msg}
		assign {slave_mail_stream_size + length}, {msg->msg_length}
		assign {inst->slave_stderr_id.id_mbox}, {msg->msg_dest.id_mbox}
		assign {inst->slave_stderr_id.id_cpu}, {msg->msg_dest.id_cpu}
		static_call sys_mem, copy, {buffer, &msg->slave_mail_stream_data, length}, {_, _}
		assign {master_state_started}, {msg->slave_mail_stream_state}
		static_call sys_mail, send, {msg}

		eval {inst}, r0
		pop_scope
		vp_ret

	fn_function_end
