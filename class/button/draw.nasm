%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/font.inc'
%include 'class/class_button.inc'

	fn_function class/button/draw
		;inputs
		;r0 = button object
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
		vp_cpy button_border_size, r3
		vp_cpy [r0 + button_state], r2
		vp_and button_state_pressed, r2
		if r2, !=, 0
			vp_mul -1, r3
		endif
		vp_cpy 1, r2
		static_call button, draw_panel

		;draw text
		vp_cpy [r4 + local_view], r0
		vp_cpy [r0 + label_text], r1
		if r1, !=, 0
			vp_cpy [r0 + label_font], r0
			static_call font, text
			if r0, !=, 0
				vp_cpy button_border_size, r8
				vp_cpy button_border_size, r9
				vp_cpy [r0 + ft_text_width], r10
				vp_cpy [r0 + ft_text_height], r11
				vp_cpy [r0 + ft_text_texture], r1
				vp_cpy [r4 + local_view], r2
				vp_cpy [r2 + button_state], r2
				vp_and button_state_pressed, r2
				if r2, !=, 0
					vp_add button_border_size, r8
					vp_add button_border_size, r9
				endif
				vp_cpy [r4 + local_view], r2
				vp_cpy [r2 + label_text_color], r2
				vp_cpy [r4 + local_ctx], r0
				static_call ctx, blit
			endif
		endif

		vp_add local_size, r4
		vp_ret

	fn_function_end
