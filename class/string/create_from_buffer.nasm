%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	def_function class/string/create_from_buffer
		;inputs
		;r0 = buffer pointer
		;r1 = buffer length
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5-r7

		;save size of data
		vp_cpy r0, r6
		vp_cpy r1, r7

		;create new string object
		s_call string, new, {&[r1 + string_size + 1]}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, string
			s_call string, init, {r0, @_function_, r6, r7}, {r1}
			if r1, ==, 0
				;error with init
				m_call string, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	def_function_end
