%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_stream_msg_out.inc'

	fn_function class/master/stop
		;inputs
		;r0 = master object
		;trashes
		;all but r0, r4

		ptr inst
		ptr msg

		push_scope
		retire {r0}, {inst}
		if {inst->master_state != stream_mail_state_stopped}
			;send normal EOF
			static_call master, input, {inst, 0, 0}

			;send master EOF
			static_call sys_mail, alloc, {}, {msg}
			assign {stream_mail_size}, {msg->msg_length}
			assign {inst->master_input_id.id_mbox}, {msg->msg_dest.id_mbox}
			assign {inst->master_input_id.id_cpu}, {msg->msg_dest.id_cpu}
			assign {inst->master_input_seqnum}, {msg->stream_mail_seqnum}
			assign {stream_mail_state_stopped}, {msg->stream_mail_state}
			static_call sys_mail, send, {msg}

			;wait for master EOF
			loop_start
				assign {0}, {msg}
				loop_start
					static_call stream_msg_out, next_seq, {&inst->master_output_list, msg, inst->master_output_seqnum}, {msg}
					breakif {msg}
					static_call sys_mail, read, {&inst->master_output_mailbox}, {msg}
				loop_end
				breakif {msg->stream_mail_state == stream_mail_state_stopped}
				assign {inst->master_output_seqnum + 1}, {inst->master_output_seqnum}
				static_call sys_mem, free, {msg}
			loop_end
			static_call sys_mem, free, {msg}

			;discard any remaining stderr
			loop_start
				static_call sys_mail, try_read, {&inst->master_error_mailbox}, {msg}
				breakif {!msg}
				static_call sys_mem, free, {msg}
			loop_end

			;stop state
			assign {stream_mail_state_stopped}, {inst->master_state}
		endif

		eval {inst}, {r0}
		pop_scope
		return

	fn_function_end
