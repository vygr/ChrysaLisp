%include "func.inc"
%include "sdl2.inc"

	fn_function "gui/gui_ctx_set_color"
		;inputs
		;r0 = ctx
		;r8 = r
		;r9 = g
		;r10 = b
		;r11 = a
		;trashes
		;r0-r3, r5-r15

		vp_cpy r4, r15
		vp_and -16, r4

		vp_cpy [r0 + GUI_CTX_SDL_CTX], r0
		sdl_setrenderdrawcolor r0, r8, r9, r10, r11

		vp_cpy r15, r4
		vp_ret

	fn_function_end
