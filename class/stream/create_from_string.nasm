%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/create_from_string
		;inputs
		;r0 = string object
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5-r6

		;create new string object
		vp_cpy r0, r6
		s_call stream, new, {}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, stream
			s_call stream, init, {r0, @_function_, r6}, {r1}
			if r1, ==, 0
				;error with init
				m_call stream, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
