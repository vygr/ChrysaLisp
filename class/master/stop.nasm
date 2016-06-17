%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_stream_msg_out.inc'
%include 'class/class_stream_msg_in.inc'

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
			;flush remaining
			method_call stream_msg_out, write_flush, {inst->master_input}

			;send stopping
			assign {stream_mail_state_stopping}, {inst->master_input->stream_msg_out_state}
			method_call stream_msg_out, write_next, {inst->master_input}
			method_call stream_msg_out, write_flush, {inst->master_input}

			;send stopped
			assign {stream_mail_state_stopped}, {inst->master_input->stream_msg_out_state}
			method_call stream_msg_out, write_next, {inst->master_input}
			method_call stream_msg_out, write_flush, {inst->master_input}

			;wait for stopped
			loop_start
				method_call stream_msg_in, read_next, {inst->master_output}, {_}
			loop_until {inst->master_output->stream_msg_in_state == stream_mail_state_stopped}

			;free input, output and error streams
			static_call stream_msg_out, deref, {inst->master_input}
			static_call stream_msg_in, deref, {inst->master_output}
			static_call stream_msg_in, deref, {inst->master_error}

			;stop state
			assign {stream_mail_state_stopped}, {inst->master_state}
		endif

		eval {inst}, {r0}
		pop_scope
		return

	fn_function_end
