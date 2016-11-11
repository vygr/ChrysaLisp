%include 'inc/func.ninc'
%include 'class/class_vector.ninc'

def_func class/vector/set_capacity
	;inputs
	;r0 = vector object
	;r1 = vector capacity
	;outputs
	;r0 = vector object
	;trashes
	;r1-r3, r5-r8

	;do we allready have room ?
	vp_cpy [r0 + vector_capacity], r2
	if r1, >, r2
		;grow the dynamic array
		vp_push r0
		vp_mul ptr_size, r1
		vp_mul ptr_size, r2
		vp_xchg r1, r2
		f_call sys_mem, grow, {[r0 + vector_array], r1, r2}, {r1, r2}
		vp_pop r0
		vp_shr 3, r2
		vp_cpy r1, [r0 + vector_array]
		vp_cpy r2, [r0 + vector_capacity]
	endif
	vp_ret

def_func_end
