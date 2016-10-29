%include 'inc/func.inc'
%include 'class/class_pair.inc'

def_func class/pair/get_second
	;inputs
	;r0 = pair object
	;outputs
	;r0 = pair object
	;r1 = object pointer

	vp_cpy [r0 + pair_second], r1
	vp_ret

def_func_end
