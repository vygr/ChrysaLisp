%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'class/class_text.inc'

	fn_function class/text/set_font
		;inputs
		;r0 = text object
		;r1 = font name
		;r2 = point size
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		;open font
		s_call gui_font, open, {r1, r2}, {r0}

		vp_cpy [r4 + local_inst], r1
		vp_cpy r0, [r1 + text_font]

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
