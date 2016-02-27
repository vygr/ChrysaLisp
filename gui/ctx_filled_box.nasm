%include "func.inc"
%include "sdl2.inc"

	struc FBOX
		FBOX_RECT:	resb SDL_RECT_SIZE
		FBOX_CTX:	resq 1
		FBOX_NEXT:	resq 1
		FBOX_SIZE:
	endstruc

	fn_function "gui/ctx_filled_box"
		;inputs
		;r0 = ctx
		;r8 = x
		;r9 = y
		;r10 = w
		;r11 = h
		;trashes
		;r0-r3, r5-r15

		vp_cpy r4, r15
		vp_sub FBOX_SIZE, r4
		vp_and -16, r4

		;save draw rectangle info
		vp_cpy r0, [r4 + FBOX_CTX]
		vp_add [r0 + GUI_CTX_X], r8
		vp_add [r0 + GUI_CTX_Y], r9
		vp_cpy r8d, [r4 + FBOX_RECT + SDL_RECT_X]
		vp_cpy r9d, [r4 + FBOX_RECT + SDL_RECT_Y]
		vp_cpy r10d, [r4 + FBOX_RECT + SDL_RECT_W]
		vp_cpy r11d, [r4 + FBOX_RECT + SDL_RECT_H]

		;for each patch on the dirty list
		vp_cpy [r0 + GUI_CTX_DIRTY_REGION], r1
		loop_list_forwards r1, r2, r1
			vp_cpy r2, [r4 + FBOX_NEXT]

			;set clip region to this patch
			vp_cpy [r1 + GUI_PATCH_X], r8
			vp_cpy [r1 + GUI_PATCH_Y], r9
			vp_cpy [r1 + GUI_PATCH_W], r10
			vp_cpy [r1 + GUI_PATCH_H], r11
			vp_cpy [r4 + FBOX_CTX], r0
			fn_call gui/ctx_set_clip

			;draw the rectangle
			vp_cpy [r4 + FBOX_CTX], r0
			vp_cpy [r0+ GUI_CTX_SDL_CTX], r0
			sdl_renderfillrect r0, r4

			vp_cpy [r4 + FBOX_NEXT], r2
		loop_end

		vp_cpy r15, r4
		vp_ret

	fn_function_end
