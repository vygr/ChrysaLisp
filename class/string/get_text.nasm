%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/get_text
		;inputs
		;r0 = string object
		;outputs
		;r0 = string object
		;r1 = string pointer

		vp_cpy [r0 + string_text], r1
		vp_ret

	fn_function_end
