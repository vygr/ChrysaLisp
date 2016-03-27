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
		;all but r0, r4

		def_structure	local
			def_long	local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		vp_cpy [r0 + string_text], r1
		vp_xor r10, r10
		vp_xor r11, r11
		if r1, !=, 0
			vp_cpy [r0 + string_font], r0
			if r0, !=, 0
				static_call gui_font, bounds
			endif
		endif

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
