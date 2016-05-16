%include 'inc/func.inc'
%include 'class/class_vector.inc'

	fn_function class/vector/set_capacity
		;inputs
		;r0 = text object
		;r1 = vector capacity
		;outputs
		;r0 = text object
		;trashes
		;all but r0, r4

		;do we allready have room ?
		vp_cpy [r0 + vector_capacity], r2
		if r1, >, r2
			;grow the dynamic array
		endif
		vp_ret

	fn_function_end
