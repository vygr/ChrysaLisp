%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'class/class_label.inc'

	fn_function class/label/pref_size
		;inputs
		;r0 = label object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r4

		vp_cpy [r0 + label_text], r1
		vp_xor r10, r10
		vp_xor r11, r11
		if r1, !=, 0
			vp_cpy [r0 + label_font], r0
			static_jmp font, bounds
		endif
		vp_ret

	fn_function_end
