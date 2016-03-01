%include "inc/func.inc"
%include "inc/gui.inc"
%include "inc/heap.inc"

	struc COPY
		COPY_NEXT:	resq 1
		COPY_SLIST:	resq 1
		COPY_DLIST:	resq 1
		COPY_CLIST:	resq 1
		COPY_SIZE:
	endstruc

	fn_function "gui/patch_list_copy"
		;inputs
		;r0 = patch heap pointer
		;r1 = source patch listhead pointer
		;r2 = dest patch listhead pointer
		;r3 = copy patch listhead pointer
		;trashes
		;r1-r3, r5-r15

		;save inputs
		vp_sub COPY_SIZE, r4
		vp_cpy r1, [r4 + COPY_SLIST]
		vp_cpy r2, [r4 + COPY_DLIST]
		vp_cpy r3, [r4 + COPY_CLIST]

		;run through copy patch list
		vp_cpy [r3], r1
		loop_while r1, !=, 0
			vp_cpy [r1 + GUI_PATCH_NEXT], r2
			vp_cpy r2, [r4 + COPY_NEXT]
			vp_cpy [r1 + GUI_PATCH_X], r8
			vp_cpy [r1 + GUI_PATCH_Y], r9
			vp_cpy [r1 + GUI_PATCH_X1], r10
			vp_cpy [r1 + GUI_PATCH_Y1], r11
			vp_cpy [r4 + COPY_SLIST], r1
			vp_cpy [r4 + COPY_DLIST], r2
			fn_call gui/patch_copy
			vp_cpy [r4 + COPY_NEXT], r1
		loop_end
		vp_add COPY_SIZE, r4
		vp_ret

	fn_function_end
