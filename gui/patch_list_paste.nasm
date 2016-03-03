%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/heap.inc'

	struc paste
		paste_next:		resq 1
		paste_dlist:	resq 1
	endstruc

	fn_function gui/patch_list_paste
		;inputs
		;r0 = patch heap pointer
		;r1 = source patch listhead pointer
		;r2 = dest patch listhead pointer
		;trashes
		;r1-r3, r5-r15

		;save inputs
		vp_sub paste_size, r4
		vp_cpy r2, [r4 + paste_dlist]

		;run through source patch list
		vp_cpy [r1], r1
		loop_while r1, !=, 0
			vp_cpy [r1 + gui_patch_next], r2
			vp_cpy r2, [r4 + paste_next]
			vp_cpy [r1 + gui_patch_x], r8
			vp_cpy [r1 + gui_patch_y], r9
			vp_cpy [r1 + gui_patch_x1], r10
			vp_cpy [r1 + gui_patch_y1], r11
			vp_cpy [r4 + paste_dlist], r1
			fn_call gui/patch_paste
			vp_cpy [r4 + paste_next], r1
		loop_end
		vp_add paste_size, r4
		vp_ret

	fn_function_end
