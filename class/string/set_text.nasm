%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/set_text
		;inputs
		;r0 = string object
		;r1 = string pointer

		vp_cpy r1, [r0 + string_text]
		vp_ret

	fn_function_end
