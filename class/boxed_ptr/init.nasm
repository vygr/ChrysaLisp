%include 'inc/func.ninc'
%include 'class/class_boxed_ptr.ninc'

def_func class/boxed_ptr/init
	;inputs
	;r0 = object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok

	;init parent
	s_call boxed_ptr, init, {r0, r1}, {r1}
	if r1, !=, 0
		;init myself
		vp_xor r2, r2
		vp_cpy r2, [r0 + boxed_ptr_value]
		vp_cpy r2, [r0 + boxed_ptr_flags]
	endif
	vp_ret

def_func_end
