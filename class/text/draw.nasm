%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/font.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'

	fn_function class/text/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		def_structure local
			long local_inst
			long local_ctx
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst [r4 + local_inst], [r4 + local_ctx]
		map_src_to_dst

		;draw text
		vp_cpy [r0 + text_string], r1
		if r1, !=, 0
			vp_cpy [r0 + text_font], r0
			if r0, !=, 0
				vp_add string_data, r1
				s_call gui_font, text, {r0, r1}, {r0}
				if r0, !=, 0
					vp_cpy [r4 + local_inst], r2
					s_call gui_ctx, blit, {[r4 + local_ctx], [r0 + ft_text_texture], [r2 + text_text_color], \
												0, 0, [r0 + ft_text_width], [r0 + ft_text_height]}
				endif
			endif
		endif

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
