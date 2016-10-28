%include 'inc/func.inc'
%include 'class/class_vector.inc'

def_func class/vector/set_element
	;inputs
	;r0 = vector object
	;r1 = object
	;r2 = vector element
	;outputs
	;r0 = vector object
	;trashes
	;all but r0, r4

	vp_push r0
	vp_mul ptr_size, r2
	vp_add [r0 + vector_array], r2
	vp_cpy [r2], r0
	vp_cpy r1, [r2]
	f_call ref, deref, {r0}
	vp_pop r0
	vp_ret

def_func_end
