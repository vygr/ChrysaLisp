%include 'inc/func.inc'
%include 'class/class_vector.inc'

	fn_function class/vector/get_capacity
		;inputs
		;r0 = text object
		;outputs
		;r0 = text object
		;r1 = vector length

		vp_cpy [r0 + vector_capacity], r0
		vp_ret

	fn_function_end
