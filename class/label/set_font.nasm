%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	fn_function class/label/set_font
		;inputs
		;r0 = label object
		;r1 = font name
		;r2 = point size
		;trashes
		;all but r0, r4

		vp_push r0
		static_call text, set_font, '[r0 + label_text], r1'
		vp_pop r0
		vp_ret

	fn_function_end
