%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/deinit
		;inputs
		;r0 = stream object
		;trashes
		;all but r0, r4

		;deref any buffer object
		vp_push r0
		vp_cpy [r0 + stream_object], r0
		if r0, !=, 0
			s_call ref, deref, {r0}
		endif

		;free any buffer
		vp_cpy [r4], r0
		s_call sys_mem, free, {[r0 + stream_buffer]}
		vp_pop r0

		;parent deinit
		p_jmp stream, deinit, {r0}

	fn_function_end
