%include 'inc/func.inc'
%include 'class/class_label.inc'

	fn_function class/label/set_text_color
		;inputs
		;r0 = label object
		;r1 = color

		vp_cpy r1, [r0 + label_text_color]
		vp_ret

	fn_function_end
