%include 'inc/func.inc'
%include 'class/class_stream.inc'

	def_function class/stream/create
		;inputs
		;r0 = object pointer, 0 if none
		;r1 = buffer pointer, 0 if none
		;r2 = buffer start
		;r3 = buffer length
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5-r8

		;create new stream object
		set_src r0, r1, r2, r3
		set_dst r5, r6, r7, r8
		map_src_to_dst

		s_call stream, new, {}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, stream
			s_call stream, init, {r0, @_function_, r5, r6, r7, r8}, {r1}
			if r1, ==, 0
				;error with init
				m_call stream, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	def_function_end
