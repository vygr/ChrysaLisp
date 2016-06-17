%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream_msg_out.inc'
%include 'class/class_stream_msg_in.inc'

	fn_function class/master/stop
		;inputs
		;r0 = master object
		;trashes
		;all but r0, r4

		ptr inst
		ptr msg
		ptr stream
		ulong length

		push_scope
		retire {r0}, {inst}
		if {inst->master_state != stream_mail_state_stopped}
			;flush remaining
			static_call master, get_input, {inst}, {stream}
			method_call stream_msg_out, write_flush, {stream}

			;send stopping
			assign {stream_mail_state_stopping}, {stream->stream_msg_out_state}
			method_call stream_msg_out, write_next, {stream}
			method_call stream_msg_out, write_flush, {stream}

			;send stopped
			assign {stream_mail_state_stopped}, {stream->stream_msg_out_state}
			method_call stream_msg_out, write_next, {stream}
			method_call stream_msg_out, write_flush, {stream}

			;wait for all stopped, starting with the pipe output, then the error streams
			static_call vector, get_length, {inst->master_streams}, {length}
			loop_start
				assign {length - 1}, {length}
				breakif {!length}
				assign {(inst->master_streams->vector_array)[length * ptr_size]}, {stream}
				loop_start
					method_call stream_msg_in, read_next, {stream}, {_}
				loop_until {stream->stream_msg_in_state == stream_mail_state_stopped}
			loop_end

			;free streams, select and mailbox array
			static_call vector, deref, {inst->master_streams}
			static_call sys_mem, free, {inst->master_select_array}
			static_call sys_mem, free, {inst->master_mailbox_array}

			;stop state
			assign {stream_mail_state_stopped}, {inst->master_state}
		endif

		eval {inst}, {r0}
		pop_scope
		return

	fn_function_end
