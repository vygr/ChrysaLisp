%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'class/class_string.inc'

	fn_function class/string/pref_size
		;inputs
		;r0 = string object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r4

		vp_cpy [r0 + string_text], r1
		vp_xor r10, r10
		vp_xor r11, r11
		if r1, !=, 0
			vp_cpy [r0 + string_font], r0
			if r0, !=, 0
				static_jmp sys_font, bounds
			endif
		endif
		vp_ret

	fn_function_end
