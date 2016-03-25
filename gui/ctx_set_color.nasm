%include 'inc/func.inc'
%include 'inc/sdl2.inc'

	fn_function gui/ctx_set_color
		;inputs
		;r0 = ctx
		;r8 = r
		;r9 = g
		;r10 = b
		;r11 = a
		;trashes
		;all but r4

		def_structure	local
			def_long	local_old_stack
		def_structure_end

		vp_cpy r4, r15
		vp_sub local_size, r4
		vp_and -16, r4
		vp_cpy r15, [r4 + local_old_stack]

		sdl_set_render_draw_color [r0 + gui_ctx_sdl_ctx], r8, r9, r10, r11

		vp_cpy [r4 + local_old_stack], r4
		vp_ret

	fn_function_end
