%include 'inc/func.inc'
%include 'class/class_string.inc'

def_func class/string/get_length
	;inputs
	;r0 = string object
	;outputs
	;r0 = string object
	;r1 = string length

	vp_cpy [r0 + string_length], r1
	vp_ret

def_func_end
