%include 'inc/func.inc'
%include 'inc/sdl2.inc'

	fn_function gui/ctx_box
		;inputs
		;r0 = ctx
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;all but r4

		def_structure	local
			def_struct	local_rect, sdl_rect
			def_struct	local_clip_rect, sdl_rect
			def_long	local_ctx
			def_long	local_dirty_rect
			def_long	local_old_stack
		def_structure_end

		;align stack to 16 bytes for SDl
		vp_cpy r4, r15
		vp_sub local_size, r4
		vp_and -16, r4
		vp_cpy r15, [r4 + local_old_stack]

		;save draw rectangle info
		vp_cpy r0, [r4 + local_ctx]
		vp_add [r0 + gui_ctx_x], r8
		vp_add [r0 + gui_ctx_y], r9
		vp_cpy_i r8, [r4 + local_rect + sdl_rect_x]
		vp_cpy_i r9, [r4 + local_rect + sdl_rect_y]
		vp_cpy_i r10, [r4 + local_rect + sdl_rect_w]
		vp_cpy_i r11, [r4 + local_rect + sdl_rect_h]

		;for each rect on the dirty region
		vp_cpy [r0 + gui_ctx_dirty_region], r0
		loop_flist_forward r0, r0, r0
			vp_cpy r0, [r4 + local_dirty_rect]

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
			vp_cpy [r4 + local_ctx], r0
			vp_lea [r4 + local_clip_rect], r1
			sdl_render_set_clip_rect [r0 + gui_ctx_sdl_ctx], r1

			;draw the rectangle
			vp_cpy [r4 + local_ctx], r0
			vp_lea [r4 + local_rect], r1
			sdl_render_draw_rect [r0 + gui_ctx_sdl_ctx], r1

			vp_cpy [r4 + local_dirty_rect], r0
		loop_end

		vp_cpy [r4 + local_old_stack], r4
		vp_ret

	fn_function_end
