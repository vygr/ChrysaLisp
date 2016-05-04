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

		def_local
			def_local_long	inst
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst .inst
		map_src_to_dst

		;open font
		s_call gui_font, open, {r1, r2}, {r0}

		vp_cpy .inst, r1
		vp_cpy r0, [r1 + text_font]

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
