%include 'inc/func.ninc'
%include 'class/class_pair.ninc'

def_func class/pair/ref_first
	;inputs
	;r0 = pair object
	;outputs
	;r0 = pair object
	;r1 = object pointer

	vp_push r0
	f_call ref, ref, {[r0 + pair_first]}
	vp_cpy r0, r1
	vp_pop r0
	vp_ret

def_func_end
