%include 'inc/func.inc'
%include 'inc/sdl2.inc'

	struc COLOR
		COLOR_OLD_STACK:	resq 1
		COLOR_SIZE:
	endstruc

	fn_function gui/ctx_set_color
		;inputs
		;r0 = ctx
		;r8 = r
		;r9 = g
		;r10 = b
		;r11 = a
		;trashes
		;r0-r3, r5-r15

		vp_cpy r4, r1
		vp_sub COLOR_SIZE, r4
		vp_and -16, r4
		vp_cpy r1, [r4 + COLOR_OLD_STACK]

		vp_cpy [r0 + GUI_CTX_SDL_CTX], r0
		sdl_setrenderdrawcolor r0, r8, r9, r10, r11

		vp_cpy [r4 + COLOR_OLD_STACK], r4
		vp_ret

	fn_function_end
