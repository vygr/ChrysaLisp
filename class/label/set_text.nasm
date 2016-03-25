%include 'inc/func.inc'
%include 'class/class_label.inc'

	fn_function class/label/set_text
		;inputs
		;r0 = label object
		;r1 = string pointer

		vp_cpy r1, [r0 + label_text]
		vp_ret

	fn_function_end
