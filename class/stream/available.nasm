%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/available
		;inputs
		;r0 = stream object
		;outputs
		;r0 = stream object
		;r1 = available space

		vp_cpy [r0 + stream_bufe], r1
		vp_sub [r0 + stream_bufp], r1
		vp_ret

	fn_function_end
