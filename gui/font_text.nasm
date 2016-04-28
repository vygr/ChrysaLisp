%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'inc/string.inc'
%include 'inc/sdl2.inc'
%include 'inc/task.inc'

	fn_function gui/font_text, no_debug_enter
		;inputs
		;r0 = font entry
		;r1 = text
		;outputs
		;r0 = 0 if error, else text entry
		;trashes
		;all but r4

		def_local
			def_local_long	font
			def_local_long	text
			def_local_long	handle
			def_local_long	surface
			def_local_long	width
			def_local_long	height
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst .font, .text
		map_src_to_dst

		;get font statics
		static_bind gui_font, statics, r5

		;search text list
		loop_flist_forward r5 + ft_statics_text_list, r5, r5
			vp_cpy .font, r0
			continueif r0, !=, [r5 + ft_text_font]
			static_call sys_string, compare, {&[r5 + ft_text_name], .text}, {r0}
		loop_until r0, !=, 0

		;did we find it ?
		vp_cpy r5, r0
		if r5, ==, 0
			;no so try create it
			static_call sys_task, callback, {$kernel_callback, r4}
			vp_cpy .handle, r0
		endif

		vp_add local_size, r4
		vp_ret

	kernel_callback:
		;inputs
		;r0 = user data
		;trashes
		;all but r4

		;align stack
		vp_cpy r4, r15
		vp_and -16, r4

		;save input
		vp_cpy r0, r14

		vp_cpy [r14 + local_font], r0
		ttf_render_utf8_blended [r0 + ft_font_handle], [r14 + local_text], 0xffffff
		if r0, !=, 0
			vp_cpy r0, [r14 + local_surface]
			vp_xor r10, r10
			vp_xor r11, r11
			vp_cpy_i [r0 + sdl_surface_w], r10
			vp_cpy_i [r0 + sdl_surface_h], r11
			vp_cpy r10, [r14 + local_width]
			vp_cpy r11, [r14 + local_height]

			;create texture
			static_bind gui_gui, statics, r0
			sdl_create_texture_from_surface [r0 + gui_statics_renderer], [r14 + local_surface]
			if r0, !=, 0
				vp_cpy r0, r5

				static_call sys_string, length, {[r14 + local_text]}, {r1}
				static_call sys_mem, alloc, {&[r1 + ft_text_size + 1]}, {r13, _}
				assert r0, !=, 0

				vp_cpy [r14 + local_font], r0
				vp_cpy r0, [r13 + ft_text_font]
				vp_cpy r5, [r13 + ft_text_texture]
				static_call sys_string, copy, {[r14 + local_text], &[r13 + ft_text_name]}, {_, _}

				;fill in width and height
				vp_cpy [r14 + local_width], r10
				vp_cpy [r14 + local_height], r11
				vp_cpy r10, [r13 + ft_text_width]
				vp_cpy r11, [r13 + ft_text_height]

				;texture blend mode
				sdl_set_texture_blend_mode [r13 + ft_text_texture], SDL_BLENDMODE_BLEND

				vp_cpy r13, r0
				static_bind gui_font, statics, r5
				ln_add_fnode r5 + ft_statics_text_list, r0, r1
			endif
			vp_cpy r0, [r14 + local_handle]
			sdl_free_surface [r14 + local_surface]
			vp_cpy [r14 + local_handle], r0
		endif
		vp_cpy r0, [r14 + local_handle]

		vp_cpy r15, r4
		vp_ret

	fn_function_end
