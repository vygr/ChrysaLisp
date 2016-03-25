%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/font.inc'
%include 'class/class_title.inc'

	fn_function class/title/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r4

		def_structure	local
			def_long	local_view
			def_long	local_ctx
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_view]
		vp_cpy r1, [r4 + local_ctx]

		;draw panel
		vp_cpy 1, r2
		vp_cpy 1, r3
		static_call title, draw_panel

		;draw text
		vp_cpy [r4 + local_view], r0
		vp_cpy [r0 + label_text], r1
		if r1, !=, 0
			vp_cpy [r0 + label_font], r0
			static_call font, text
			if r0, !=, 0
				vp_cpy 1, r8
				vp_cpy 1, r9
				vp_cpy [r0 + ft_text_width], r10
				vp_cpy [r0 + ft_text_height], r11
				vp_cpy [r0 + ft_text_texture], r1
				vp_cpy [r4 + local_view], r2
				vp_cpy [r2 + label_text_color], r2
				vp_cpy [r4 + local_ctx], r0
				static_call ctx, blit
			endif
		endif

		vp_add local_size, r4
		vp_ret

	fn_function_end
