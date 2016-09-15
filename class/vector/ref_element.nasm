%include 'inc/func.inc'
%include 'class/class_vector.inc'

	fn_function class/vector/ref_element
		;inputs
		;r0 = vector object
		;r1 = vector element
		;outputs
		;r0 = vector object
		;r1 = object

		vp_push r0
		vp_mul ptr_size, r1
		vp_cpy [r0 + vector_array], r0
		s_call ref, ref, {[r0 + r1]}
		vp_cpy r0, r1
		vp_pop r0
		vp_ret

	fn_function_end
