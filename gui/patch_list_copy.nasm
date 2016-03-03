%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/heap.inc'

	struc copy
		copy_next:	resq 1
		copy_slist:	resq 1
		copy_dlist:	resq 1
		copy_clist:	resq 1
	endstruc

	fn_function gui/patch_list_copy
		;inputs
		;r0 = patch heap pointer
		;r1 = source patch listhead pointer
		;r2 = dest patch listhead pointer
		;r3 = copy patch listhead pointer
		;trashes
		;r1-r3, r5-r15

		;save inputs
		vp_sub copy_size, r4
		vp_cpy r1, [r4 + copy_slist]
		vp_cpy r2, [r4 + copy_dlist]
		vp_cpy r3, [r4 + copy_clist]

		;run through copy patch list
		vp_cpy [r3], r1
		loop_while r1, !=, 0
			vp_cpy [r1 + gui_patch_next], r2
			vp_cpy r2, [r4 + copy_next]
			vp_cpy [r1 + gui_patch_x], r8
			vp_cpy [r1 + gui_patch_y], r9
			vp_cpy [r1 + gui_patch_x1], r10
			vp_cpy [r1 + gui_patch_y1], r11
			vp_cpy [r4 + copy_slist], r1
			vp_cpy [r4 + copy_dlist], r2
			fn_call gui/patch_copy
			vp_cpy [r4 + copy_next], r1
		loop_end
		vp_add copy_size, r4
		vp_ret

	fn_function_end
