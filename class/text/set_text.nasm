%include 'inc/func.inc'
%include 'class/class_text.inc'

	fn_function class/text/set_text
		;inputs
		;r0 = text object
		;r1 = string pointer

		vp_cpy r1, [r0 + text_text]
		vp_ret

	fn_function_end
