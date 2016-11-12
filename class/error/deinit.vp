%include 'inc/func.ninc'
%include 'class/class_error.ninc'

def_func class/error/deinit
	;inputs
	;r0 = error object
	;trashes
	;all but r0, r4

	vp_push r0
	f_call ref, deref, {[r0 + error_description]}
	vp_cpy [r4], r0
	f_call ref, deref, {[r0 + error_object]}
	vp_pop r0
	s_jmp error, deinit, {r0}

def_func_end
