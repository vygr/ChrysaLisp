%include 'inc/func.inc'
%include 'inc/sdl2.inc'

	fn_function gui/ctx_blit
		;inputs
		;r0 = ctx
		;r1 = texture
		;r2 = color mod
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;all but r4

		def_local
			def_local_struct	drect, sdl_rect
			def_local_struct	srect, sdl_rect
			def_local_struct	clip_rect, sdl_rect
			def_local_long		ctx
			def_local_long		texture
			def_local_long		dirty_rect
			def_local_long		old_stack
		def_local_end

		;align stack to 16 bytes for SDl
		vp_cpy r4, r15
		vp_sub local_size, r4
		vp_and -16, r4
		vp_cpy r15, .old_stack

		;save draw rectangle info
		vp_cpy r0, .ctx
		vp_cpy r1, .texture
		vp_add [r0 + gui_ctx_x], r8
		vp_add [r0 + gui_ctx_y], r9
		vp_cpy_i r8, [r4 + local_drect + sdl_rect_x]
		vp_cpy_i r9, [r4 + local_drect + sdl_rect_y]
		vp_cpy_i r10, [r4 + local_drect + sdl_rect_w]
		vp_cpy_i r11, [r4 + local_drect + sdl_rect_h]
		vp_xor r8, r8
		vp_cpy_i r8, [r4 + local_srect + sdl_rect_x]
		vp_cpy_i r8, [r4 + local_srect + sdl_rect_y]
		vp_cpy_i r10, [r4 + local_srect + sdl_rect_w]
		vp_cpy_i r11, [r4 + local_srect + sdl_rect_h]

		;set the color mod
		vp_cpy r2, r3
		vp_cpy r2, r1
		vp_shr 16, r3
		vp_shr 8, r2
		vp_and 0xff, r3
		vp_and 0xff, r2
		vp_and 0xff, r1
		sdl_set_texture_color_mod .texture, r3, r2, r1

		;for each rect on the dirty region
		vp_cpy .ctx, r0
		vp_cpy [r0 + gui_ctx_dirty_region], r0
		loop_flist_forward r0, r0, r0
			vp_cpy r0, .dirty_rect

			;set clip region to this region
			vp_cpy [r0 + gui_rect_x], r8
			vp_cpy [r0 + gui_rect_y], r9
			vp_cpy [r0 + gui_rect_x1], r10
			vp_cpy [r0 + gui_rect_y1], r11
			vp_sub r8, r10
			vp_sub r9, r11
			vp_cpy_i r8, [r4 + local_clip_rect + sdl_rect_x]
			vp_cpy_i r9, [r4 + local_clip_rect + sdl_rect_y]
			vp_cpy_i r10, [r4 + local_clip_rect + sdl_rect_w]
			vp_cpy_i r11, [r4 + local_clip_rect + sdl_rect_h]
			vp_cpy .ctx, r0
			sdl_render_set_clip_rect [r0 + gui_ctx_sdl_ctx], &.clip_rect

			;blit the texture
			vp_cpy .ctx, r0
			sdl_render_copy [r0 + gui_ctx_sdl_ctx], .texture, &.srect, &.drect

			vp_cpy .dirty_rect, r0
		loop_end

		vp_cpy .old_stack, r4
		vp_ret

	fn_function_end
