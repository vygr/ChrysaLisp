%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/skip_not
		;inputs
		;r0 = stream object
		;r1 = char to not skip
		;trashes
		;r2-r3

		vp_cpy [r0 + stream_bufp], r2
		loop_while r2, ==, [r0 + stream_bufe]
			vp_cpy_ub [r2], r3
			breakif r3, ==, r1
			vp_inc r2
		loop_end
		vp_cpy r2, [r0 + stream_bufp]
		vp_ret

	fn_function_end
