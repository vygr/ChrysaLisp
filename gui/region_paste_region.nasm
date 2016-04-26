%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function gui/region_paste_region
		;inputs
		;r0 = region heap pointer
		;r1 = source region listhead pointer
		;r2 = dest region listhead pointer
		;r8 = x translation
		;r9 = y translation
		;trashes
		;r1-r3, r5-r15

		def_local
			def_local_long	node
			def_local_long	dlist
			def_local_long	dx
			def_local_long	dy
		def_local_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r2, .dlist
		vp_cpy r8, .dx
		vp_cpy r9, .dy

		;run through source region list
		loop_flist_forward r1, r1, r1
			vp_cpy r1, .node

			vp_cpy [r1 + gui_rect_x], r8
			vp_cpy [r1 + gui_rect_y], r9
			vp_cpy [r1 + gui_rect_x1], r10
			vp_cpy [r1 + gui_rect_y1], r11
			vp_cpy .dx, r12
			vp_cpy .dy, r13
			vp_add r12, r8
			vp_add r13, r9
			vp_add r12, r10
			vp_add r13, r11
			static_call gui_region, paste_rect, {r0, .dlist, r8, r9, r10, r11}

			vp_cpy .node, r1
		loop_end
		vp_add local_size, r4
		vp_ret

	fn_function_end
