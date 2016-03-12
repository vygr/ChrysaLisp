%include 'inc/func.inc'
%include 'inc/sdl2.inc'

	def_structure color
		def_long	color_old_stack
	def_structure_end

	fn_function gui/ctx_set_color
		;inputs
		;r0 = ctx
		;r8 = r
		;r9 = g
		;r10 = b
		;r11 = a
		;trashes
		;all but r4

		vp_cpy r4, r1
		vp_sub color_size, r4
		vp_and -16, r4
		vp_cpy r1, [r4 + color_old_stack]

		sdl_setrenderdrawcolor [r0 + gui_ctx_sdl_ctx], r8, r9, r10, r11

		vp_cpy [r4 + color_old_stack], r4
		vp_ret

	fn_function_end
