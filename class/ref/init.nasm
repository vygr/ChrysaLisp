%include 'inc/func.ninc'
%include 'class/class_ref.ninc'

def_func class/ref/init
	;inputs
	;r0 = object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok

	;init parent
	s_call ref, init, {r0, r1}, {r1}
	if r1, !=, 0
		;init myself
		vp_cpy 1, r1
		vp_cpy r1, [r0 + ref_count]
	endif
	vp_ret

def_func_end
