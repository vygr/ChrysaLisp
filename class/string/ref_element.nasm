%include 'inc/func.ninc'
%include 'class/class_string.ninc'

def_func class/string/ref_element
	;inputs
	;r0 = string object
	;r1 = char index
	;outputs
	;r0 = string object
	;r1 = char string

	vp_push r0
	vp_add string_data, r1
	f_call string, create_from_buffer, {&[r0 + r1], 1}, {r1}
	vp_pop r0
	vp_ret

def_func_end
