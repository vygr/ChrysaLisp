%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'

	fn_function class/stream_msg_out/set_state
		;inputs
		;r0 = stream_msg_out object
		;r1 = stream state

		vp_cpy r1, [r0 + stream_msg_out_state]
		vp_ret

	fn_function_end
