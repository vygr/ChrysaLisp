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
			method_call stream_msg_out, write_next, {inst->master_input}
			method_call stream_msg_out, write_next, {inst->master_input}

			;send master EOF
			assign {stream_mail_state_stopped}, {inst->master_input->stream_msg_out_state}
			method_call stream_msg_out, write_flush, {inst->master_input}

			;free input stream
			static_call stream_msg_out, deref, {inst->master_input}

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
