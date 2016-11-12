%include 'inc/func.ninc'
%include 'class/class_vector.ninc'

def_func class/vector/init
	;inputs
	;r0 = vector object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok
	;trashes
	;all but r0, r4

	;init parent
	s_call vector, init, {r0, r1}, {r1}
	if r1, !=, 0
		;init myself
		vp_xor r2, r2
		vp_cpy r2, [r0 + vector_array]
		vp_cpy r2, [r0 + vector_length]
		vp_cpy r2, [r0 + vector_capacity]
	endif
	vp_ret

def_func_end
