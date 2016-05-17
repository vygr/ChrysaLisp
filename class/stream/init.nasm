%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/init
		;inputs
		;r0 = stream object
		;r1 = vtable pointer
		;r2 = string object
		;outputs
		;r1 = 0 if error, else ok

		;save inputs
		set_src r2
		set_dst [r0 + stream_object]
		map_src_to_dst

		;init parent
		p_call stream, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
			vp_cpy [r0 + stream_object], r2
			vp_lea [r2 + string_data], r1
			vp_cpy r1, [r0 + stream_bufp]
			vp_add [r2 + string_length], r1
			vp_cpy r1, [r0 + stream_bufe]
		endif
		vp_ret

	fn_function_end
