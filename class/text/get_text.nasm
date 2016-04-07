%include 'inc/func.inc'
%include 'class/class_text.inc'

	fn_function class/text/get_text
		;inputs
		;r0 = text object
		;outputs
		;r0 = text object
		;r1 = string pointer

		vp_cpy [r0 + text_text], r1
		vp_ret

	fn_function_end
