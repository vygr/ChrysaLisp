%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/read_line
		;inputs
		;r0 = stream object
		;r1 = buffer
		;r2 = buffer length
		;outputs
		;r0 = stream object
		;r1 = bytes read
		;trashes
		;r2-r3, r5-r6

		;save inputs
		set_src r1, r2
		set_dst r5, r6
		map_src_to_dst

		vp_xor r3, r3
		loop_while r3, !=, r6
			s_call stream, read_char, {r0}, {r1}
			continueif r1, ==, '\r'
			breakif r1, ==, '\n'
			breakif r1, ==, -1
			vp_cpy_ub r1, [r5 + r3]
			vp_inc r3
		loop_end

		vp_cpy r3, r1
		vp_ret

	fn_function_end
