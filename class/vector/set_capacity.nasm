%include 'inc/func.inc'
%include 'class/class_vector.inc'

	fn_function class/vector/set_capacity
		;inputs
		;r0 = vector object
		;r1 = vector capacity
		;outputs
		;r0 = vector object
		;trashes
		;r1-r3, r5-r8

		;do we allready have room ?
		vp_mul ptr_size, r1
		vp_cpy [r0 + vector_capacity], r2
		if r1, >, r2
			;grow the dynamic array
			vp_push r0
			vp_xchg r1, r2
			s_call sys_mem, grow, {[r0 + vector_array], r1, r2}, {r1, r2}
			vp_pop r0
			vp_cpy r1, [r0 + vector_array]
			vp_cpy r2, [r0 + vector_capacity]
		endif
		vp_ret

	fn_function_end
