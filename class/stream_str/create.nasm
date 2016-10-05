%include 'inc/func.inc'
%include 'class/class_stream_str.inc'

	def_function class/stream_str/create
		;inputs
		;r0 = string object
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5

		;create new stream_str object
		set_src r0
		set_dst r5
		map_src_to_dst

		s_call stream_str, new, {}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, stream_str
			s_call stream_str, init, {r0, @_function_, r5}, {r1}
			if r1, ==, 0
				;error with init
				m_call stream_str, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	def_function_end
