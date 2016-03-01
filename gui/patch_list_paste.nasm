%include "inc/func.inc"
%include "inc/gui.inc"
%include "inc/heap.inc"

	struc PASTE
		PASTE_NEXT:		resq 1
		PASTE_DLIST:	resq 1
		PASTE_SIZE:
	endstruc

	fn_function "gui/patch_list_paste"
		;inputs
		;r0 = patch heap pointer
		;r1 = source patch listhead pointer
		;r2 = dest patch listhead pointer
		;trashes
		;r1-r3, r5-r15

		;save inputs
		vp_sub PASTE_SIZE, r4
		vp_cpy r2, [r4 + PASTE_DLIST]

		;run through source patch list
		vp_cpy [r1], r1
		loop_while r1, !=, 0
			vp_cpy [r1 + GUI_PATCH_NEXT], r2
			vp_cpy r2, [r4 + PASTE_NEXT]
			vp_cpy [r1 + GUI_PATCH_X], r8
			vp_cpy [r1 + GUI_PATCH_Y], r9
			vp_cpy [r1 + GUI_PATCH_X1], r10
			vp_cpy [r1 + GUI_PATCH_Y1], r11
			vp_cpy [r4 + PASTE_DLIST], r1
			fn_call gui/patch_paste
			vp_cpy [r4 + PASTE_NEXT], r1
		loop_end
		vp_add PASTE_SIZE, r4
		vp_ret

	fn_function_end
