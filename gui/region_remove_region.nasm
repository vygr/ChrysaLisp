%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/heap.inc'

	struc remove
		remove_next:	resq 1
		remove_dlist:	resq 1
		remove_dx:		resq 1
		remove_dy:		resq 1
	endstruc

	fn_function gui/region_remove_region
		;inputs
		;r0 = region heap pointer
		;r1 = source region listhead pointer
		;r2 = dest region listhead pointer
		;r8 = x translation
		;r9 = y translation
		;trashes
		;r1-r3, r5-r15

		;save inputs
		vp_sub remove_size, r4
		vp_cpy r2, [r4 + remove_dlist]
		vp_cpy r8, [r4 + remove_dx]
		vp_cpy r9, [r4 + remove_dy]

		;run through source region list
		vp_cpy [r1], r1
		loop_while r1, !=, 0
			vp_cpy [r1 + gui_rect_next], r2
			vp_cpy r2, [r4 + remove_next]
			vp_cpy [r1 + gui_rect_x], r8
			vp_cpy [r1 + gui_rect_y], r9
			vp_cpy [r1 + gui_rect_x1], r10
			vp_cpy [r1 + gui_rect_y1], r11
			vp_cpy [r4 + remove_dx], r12
			vp_cpy [r4 + remove_dy], r13
			vp_add r12, r8
			vp_add r13, r9
			vp_add r12, r10
			vp_add r13, r11
			vp_cpy [r4 + remove_dlist], r1
			static_call region, remove
			vp_cpy [r4 + remove_next], r1
		loop_end
		vp_add remove_size, r4
		vp_ret

	fn_function_end
