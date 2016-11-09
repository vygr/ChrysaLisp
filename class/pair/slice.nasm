%include 'inc/func.inc'
%include 'class/class_pair.inc'

def_func class/pair/slice
	;inputs
	;r0 = pair object
	;r1 = pair element start
	;r2 = pair element end
	;outputs
	;r0 = pair object
	;r1 = slice pair object

	f_call pair, ref, {r0}
	vp_cpy r0, r1
	vp_ret

def_func_end
