%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'

	fn_function class/stream_msg_out/create
		;inputs
		;r0 = target mailbox id
		;r1 = target mailbox id
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5-r6

		;create new stream_msg_out object
		set_src r0, r1
		set_dst r5, r6
		map_src_to_dst

		s_call stream_msg_out, new, {}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, stream_msg_out
			s_call stream_msg_out, init, {r0, @_function_, r5, r6}, {r1}
			if r1, ==, 0
				;error with init
				m_call stream_msg_out, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
