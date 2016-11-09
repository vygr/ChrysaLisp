%include 'inc/func.inc'
%include 'class/class_pair.inc'

def_func class/pair/ref_element
	;inputs
	;r0 = pair object
	;r1 = pair element
	;outputs
	;r0 = pair object
	;r1 = object

	vp_push r0
	vp_add pair_first, r0
	f_call ref, ref, {[r0 + (r1 * ptr_size)]}
	vp_cpy r0, r1
	vp_pop r0
	vp_ret

def_func_end
