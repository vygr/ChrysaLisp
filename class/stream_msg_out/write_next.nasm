%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'

	def_function class/stream_msg_out/write_next
		;inputs
		;r0 = stream_msg_out object
		;outputs
		;r0 = stream_msg_out object
		;trashes
		;all but r0, r4

		ptr inst, msg

		push_scope
		retire {r0}, {inst}

		method_call stream, write_flush, {inst}
		static_call sys_mail, alloc, {}, {msg}
		assign {msg_size}, {msg->msg_length}
		assign {&msg->stream_mail_data}, {inst->stream_bufp}
		assign {&msg->msg_size}, {inst->stream_bufe}
		assign {msg}, {inst->stream_buffer}

		eval {inst}, {r0}
		pop_scope
		return

	def_function_end
