%include 'inc/func.inc'
%include 'class/class_vector.inc'

	fn_function class/vector/pop_back
		;inputs
		;r0 = vector object
		;outputs
		;r0 = vector object
		;trashes
		;all but r0, r4

		vp_push r0
		vp_cpy [r0 + vector_length], r1
		vp_sub ptr_size, r1
		vp_cpy r1, [r0 + vector_length]
		s_call ref, deref, {[r0 + r1]}
		vp_pop r0
		vp_ret

	fn_function_end
