%include 'inc/func.ninc'
%include 'class/class_vector.ninc'

def_func class/vector/get_back
	;inputs
	;r0 = vector object
	;outputs
	;r0 = vector object
	;r1 = object pointer

	vp_cpy [r0 + vector_length], r1
	vp_mul ptr_size, r1
	vp_add [r0 + vector_array], r1
	vp_cpy [r1 - ptr_size], r1
	vp_ret

def_func_end
