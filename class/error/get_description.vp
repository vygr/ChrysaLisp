%include 'inc/func.ninc'
%include 'class/class_error.ninc'

def_func class/error/get_description
	;inputs
	;r0 = error object
	;outputs
	;r0 = error object
	;r1 = string object

	vp_cpy [r0 + error_description], r1
	vp_ret

def_func_end
