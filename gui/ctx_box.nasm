%include 'inc/func.inc'
%include 'inc/sdl2.inc'

	def_structure fbox
		def_struct	fbox_rect, sdl_rect
		def_struct	fbox_clip_rect, sdl_rect
		def_long	fbox_ctx
		def_long	fbox_patch
		def_long	fbox_old_stack
	def_structure_end

	fn_function gui/ctx_box
		;inputs
		;r0 = ctx
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;all but r4

		vp_cpy r4, r1
		vp_sub fbox_size, r4
		vp_and -16, r4
		vp_cpy r1, [r4 + fbox_old_stack]

		;save draw rectangle info
		vp_cpy r0, [r4 + fbox_ctx]
		vp_add [r0 + gui_ctx_x], r8
		vp_add [r0 + gui_ctx_y], r9
		vp_cpy r8d, [r4 + fbox_rect + sdl_rect_x]
		vp_cpy r9d, [r4 + fbox_rect + sdl_rect_y]
		vp_cpy r10d, [r4 + fbox_rect + sdl_rect_w]
		vp_cpy r11d, [r4 + fbox_rect + sdl_rect_h]

		;for each region on the dirty region
		vp_cpy [r0 + gui_ctx_dirty_region], r0
		loop_start
			vp_cpy [r0 + gui_rect_next], r0
			breakif r0, ==, 0
			vp_cpy r0, [r4 + fbox_patch]

			;set clip region to this region
			vp_cpy [r0 + gui_rect_x], r8
			vp_cpy [r0 + gui_rect_y], r9
			vp_cpy [r0 + gui_rect_x1], r10
			vp_cpy [r0 + gui_rect_y1], r11
			vp_sub r8, r10
			vp_sub r9, r11
			vp_cpy r8d, [r4 + fbox_clip_rect + sdl_rect_x]
			vp_cpy r9d, [r4 + fbox_clip_rect + sdl_rect_y]
			vp_cpy r10d, [r4 + fbox_clip_rect + sdl_rect_w]
			vp_cpy r11d, [r4 + fbox_clip_rect + sdl_rect_h]
			vp_cpy [r4 + fbox_ctx], r0
			vp_lea [r4 + fbox_clip_rect], r1
			sdl_rendersetcliprect [r0+ gui_ctx_sdl_ctx], r1

			;draw the rectangle
			vp_cpy [r4 + fbox_ctx], r0
			vp_lea [r4 + fbox_rect], r1
			sdl_renderdrawrect [r0+ gui_ctx_sdl_ctx], r1

			vp_cpy [r4 + fbox_patch], r0
		loop_end

		vp_cpy [r4 + fbox_old_stack], r4
		vp_ret

	fn_function_end
