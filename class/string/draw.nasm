%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/font.inc'
%include 'class/class_string.inc'

	fn_function class/string/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
			def_long	local_ctx
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]
		vp_cpy r1, [r4 + local_ctx]

		;draw text
		vp_cpy [r0 + string_text], r1
		if r1, !=, 0
			vp_cpy [r0 + string_font], r0
			if r0, !=, 0
				static_call sys_font, text
				if r0, !=, 0
					vp_xor r8, r8
					vp_xor r9, r9
					vp_cpy [r0 + ft_text_width], r10
					vp_cpy [r0 + ft_text_height], r11
					vp_cpy [r0 + ft_text_texture], r1
					vp_cpy [r4 + local_inst], r2
					vp_cpy [r2 + string_text_color], r2
					vp_cpy [r4 + local_ctx], r0
					static_call gui_ctx, blit
				endif
			endif
		endif

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
