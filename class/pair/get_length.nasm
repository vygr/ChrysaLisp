%include 'inc/func.ninc'
%include 'class/class_pair.ninc'

def_func class/pair/get_length
	;inputs
	;r0 = pair object
	;outputs
	;r0 = pair object
	;r1 = pair length

	vp_cpy 2, r1
	vp_ret

def_func_end
