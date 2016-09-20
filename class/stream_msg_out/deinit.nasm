%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'

	def_function class/stream_msg_out/deinit
		;inputs
		;r0 = stream_msg_out object
		;trashes
		;all but r0, r4

		ptr inst, msg

		push_scope
		retire {r0}, {inst}

		;wait for final ack
		static_call sys_mail, read, {&inst->stream_msg_out_ack_mailbox}, {msg}
		static_call sys_mem, free, {msg}

		eval {inst}, {r0}
		pop_scope
		return

	def_function_end
