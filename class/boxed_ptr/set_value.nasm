%include 'inc/func.ninc'
%include 'class/class_boxed_ptr.ninc'

def_func class/boxed_ptr/set_value
	;inputs
	;r0 = object
	;r1 = value
	;outputs
	;r0 = object

	vp_cpy r1, [r0 + boxed_ptr_value]
	vp_ret

def_func_end
