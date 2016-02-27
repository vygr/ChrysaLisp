%include "func.inc"
%include "sdl2.inc"

	struc CLIP
		CLIP_RECT:		resb SDL_RECT_SIZE
		CLIP_OLD_STACK:	resq 1
		CLIP_SIZE:
	endstruc

	fn_function "gui/ctx_set_clip"
		;inputs
		;r0 = ctx
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;r0-r3, r5-r15

		vp_cpy r4, r1
		vp_sub CLIP_SIZE, r4
		vp_and -16, r4
		vp_cpy r1, [r4 + CLIP_OLD_STACK]

		vp_add [r0 + GUI_CTX_X], r8
		vp_add [r0 + GUI_CTX_Y], r9

		vp_cpy r8d, [r4 + SDL_RECT_X]
		vp_cpy r9d, [r4 + SDL_RECT_Y]
		vp_cpy r10d, [r4 + SDL_RECT_W]
		vp_cpy r11d, [r4 + SDL_RECT_H]
		vp_cpy [r0 + GUI_CTX_SDL_CTX], r0
		sdl_rendersetcliprect r0, r4

		vp_cpy [r4 + CLIP_OLD_STACK], r4
		vp_ret

	fn_function_end
