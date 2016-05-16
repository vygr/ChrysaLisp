%include 'inc/func.inc'
%include 'class/class_vector.inc'

	fn_function class/vector/get_length
		;inputs
		;r0 = vector object
		;outputs
		;r0 = vector object
		;r1 = vector length

		vp_cpy [r0 + vector_length], r1
		vp_shr 3, r1
		vp_ret

	fn_function_end
