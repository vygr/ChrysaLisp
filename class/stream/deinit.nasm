%include 'inc/func.inc'
%include 'class/class_stream.inc'

	fn_function class/stream/deinit
		;inputs
		;r0 = stream object
		;trashes
		;all but r0, r4

		;deref object
		vp_push r0
		s_call ref, deref, {[r0 + stream_object]}
		vp_pop r0

		;parent deinit
		p_jmp stream, deinit, {r0}

	fn_function_end
