%include 'inc/func.ninc'
%include 'class/class_unordered_set.ninc'
%include 'class/class_vector.ninc'

def_func class/unordered_set/ref_element
	;inputs
	;r0 = unordered_set object
	;r1 = element index
	;outputs
	;r0 = unordered_set object
	;r1 = element
	;trashes
	;r2-r3, r5

	vp_push r0
	f_call unordered_set, get_iter, {r0, r1}, {r1, _}
	f_call ref, ref, {[r1]}
	vp_cpy r0, r1
	vp_pop r0
	vp_ret

def_func_end
