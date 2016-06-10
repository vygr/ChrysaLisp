%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/get_length
		;inputs
		;r0 = string object
		;outputs
		;r0 = string object
		;r1 = string length

		vp_cpy [r0 + string_length], r1
		vp_ret

	fn_function_end
