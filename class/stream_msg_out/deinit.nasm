%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'

	fn_function class/stream_msg_out/deinit
		;inputs
		;r0 = stream_msg_out object
		;trashes
		;all but r0, r4

		ptr inst, msg

		push_scope
		retire {r0}, {inst}

		;wait for final ack
		loop_while {inst->stream_msg_out_seqnum != inst->stream_msg_out_ack_seqnum}
			assign {0}, {msg}
			loop_start
				static_call stream_msg_out, next_seq, {inst->stream_msg_out_ack_list, msg, \
													inst->stream_msg_out_ack_seqnum}, {msg}
				breakif {msg}
				static_call sys_mail, read, {inst->stream_msg_out_ack_mailbox}, {msg}
			loop_end
			static_call sys_mem, free, {msg}
			assign {inst->stream_msg_out_ack_seqnum + 1}, {inst->stream_msg_out_ack_seqnum}
		loop_end

		eval {inst}, {r0}
		pop_scope
		return

	fn_function_end
