%include 'inc/func.inc'
%include 'class/class_text.inc'

	fn_function class/text/set_text_color
		;inputs
		;r0 = text object
		;r1 = color

		vp_cpy r1, [r0 + text_text_color]
		vp_ret

	fn_function_end
