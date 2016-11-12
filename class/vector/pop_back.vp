%include 'inc/func.ninc'
%include 'class/class_vector.ninc'

def_func class/vector/pop_back
	;inputs
	;r0 = vector object
	;outputs
	;r0 = vector object
	;trashes
	;all but r0, r4

	vp_push r0
	vp_cpy [r0 + vector_length], r1
	vp_dec r1
	vp_cpy r1, [r0 + vector_length]
	vp_cpy [r0 + vector_array], r0
	f_call ref, deref, {[r0 + (r1 * ptr_size)]}
	vp_pop r0
	vp_ret

def_func_end
