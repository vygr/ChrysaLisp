%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'

	fn_function class/stream_msg_out/write_flush
		;inputs
		;r0 = stream_msg_out object
		;outputs
		;r0 = stream_msg_out object
		;trashes
		;all but r0, r4

		ptr inst
		ptr msg

		push_scope
		retire {r0}, {inst}

		assign {inst->stream_buffer}, {msg}
		if {msg}
			assign {inst->stream_bufp - msg}, {msg->msg_length}
			assign {inst->stream_msg_out_id.id_mbox}, {msg->msg_dest.id_mbox}
			assign {inst->stream_msg_out_id.id_cpu}, {msg->msg_dest.id_cpu}
			assign {inst->stream_msg_out_seqnum}, {msg->stream_mail_seqnum}
			assign {inst->stream_msg_out_state}, {msg->stream_mail_state}
			static_call sys_mail, send, {msg}
			assign {inst->stream_msg_out_seqnum + 1}, {inst->stream_msg_out_seqnum}
			assign {0}, {inst->stream_buffer}
			super_call stream_msg_out, write_flush, {inst}
		endif

		eval {inst}, {r0}
		pop_scope
		return

	fn_function_end
