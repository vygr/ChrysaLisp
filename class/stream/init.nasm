%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/init
		;inputs
		;r0 = stream object
		;r1 = vtable pointer
		;r2 = buffer object, 0 if none
		;r3 = buffer pointer, 0 if none
		;r5 = buffer start
		;r6 = buffer length
		;outputs
		;r1 = 0 if error, else ok
		;trashes
		;all but r0, r4

		;save inputs
		vp_add r5, r6
		set_src r2, r3, r5, r6
		set_dst [r0 + stream_object], [r0 + stream_buffer], [r0 + stream_bufp], [r0 + stream_bufe]
		map_src_to_dst

		;init parent
		p_call stream, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
		endif
		vp_ret

	fn_function_end
