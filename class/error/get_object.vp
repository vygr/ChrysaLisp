%include 'inc/func.ninc'
%include 'class/class_error.ninc'

def_func class/error/get_object
	;inputs
	;r0 = error object
	;outputs
	;r0 = error object
	;r1 = error payload object

	vp_cpy [r0 + error_object], r1
	vp_ret

def_func_end
