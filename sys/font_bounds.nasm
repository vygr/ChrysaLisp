%include 'inc/func.inc'
%include 'inc/font.inc'

	fn_function sys/font_bounds, no_debug_enter
		;inputs
		;r0 = font entry
		;r1 = text
		;outputs
		;r0 = 0 if error, else text entry
		;r10 = width
		;r11 = height
		;trashes
		;r1-r3, r5-r6

		static_call sys_font, text
		if r0, ==, 0
			vp_xor r10, r10
			vp_xor r11, r11
		else
			vp_cpy [r0 + ft_text_width], r10
			vp_cpy [r0 + ft_text_height], r11
		endif
		vp_ret

	fn_function_end
