%include 'inc/func.ninc'
%include 'class/class_unordered_set.ninc'
%include 'class/class_vector.ninc'

def_func class/unordered_set/get_length
	;inputs
	;r0 = unordered_set object
	;outputs
	;r0 = unordered_set object
	;r1 = length

	vp_cpy [r0 + unordered_set_length], r1
	vp_ret

def_func_end
