%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/read_char
		;inputs
		;r0 = stream object
		;outputs
		;r0 = stream object
		;r1 = -1, else char read
		;trashes
		;r2

		vp_cpy [r0 + stream_bufp], r2
		if r2, ==, [r0 + stream_bufe]
			;eof
			vp_cpy -1, r1
		else
			vp_cpy_ub [r2], r1
			vp_inc r2
			vp_cpy r2, [r0 + stream_bufp]
		endif
		vp_ret

	fn_function_end
