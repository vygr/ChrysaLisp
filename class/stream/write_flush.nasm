%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/write_flush
		;inputs
		;r0 = stream object
		;outputs
		;r0 = stream object
		;trashes
		;all but r0, r4

		vp_xor r1, r1
		vp_cpy r1, [r0 + stream_bufp]
		vp_cpy r1, [r0 + stream_bufe]
		vp_ret

	fn_function_end
