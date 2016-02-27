%include "func.inc"
%include "sdl2.inc"

	fn_function "gui/gui_set_clip"
		;inputs
		;r0 = ctx
		;r8 = x
		;r9 = y
		;r10 = w
		;r11 = h
		;trashes
		;r0-r3, r5-r15

		vp_cpy r4, r15
		vp_and -16, r4

		vp_sub SDL_RECT_SIZE, r4
		vp_cpy r8d, [r4 + SDL_RECT_X]
		vp_cpy r9d, [r4 + SDL_RECT_Y]
		vp_cpy r10d, [r4 + SDL_RECT_W]
		vp_cpy r11d, [r4 + SDL_RECT_H]
		sdl_rendersetcliprect r0, r4

		vp_cpy r15, r4
		vp_ret

	fn_function_end
