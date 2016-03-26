%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/set_text_color
		;inputs
		;r0 = string object
		;r1 = color

		vp_cpy r1, [r0 + string_text_color]
		vp_ret

	fn_function_end
