%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_stream_msg_out.inc'

	fn_function class/master/output
		;inputs
		;r0 = master object
		;outputs
		;r0 = master object
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
			static_call stream_msg_out, next_seq, {&inst->master_output_list, msg, inst->master_output_seqnum}, {msg}
			breakif {msg}
			static_call sys_mail, read, {&inst->master_output_mailbox}, {msg}
		loop_end
		assign {inst->master_output_seqnum + 1}, {inst->master_output_seqnum}

		static_call stream, create, {0, msg, &msg->stream_mail_data, msg->msg_length - stream_mail_size}, {stream}
		static_call stream, available, {stream}, {length}
		if {!length}
			static_call stream, deref, {stream}
			assign {0}, {stream}
		endif

		eval {inst, stream}, {r0, r1}
		pop_scope
		return

	fn_function_end
