%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/heap.inc'

	def_structure copy
		def_long	copy_next
		def_long	copy_slist
		def_long	copy_dlist
		def_long	copy_clist
		def_long	copy_dx
		def_long	copy_dy
	def_structure_end

	fn_function gui/region_copy_region
		;inputs
		;r0 = region heap pointer
		;r1 = source region listhead pointer
		;r2 = dest region listhead pointer
		;r3 = copy region listhead pointer
		;r8 = x translation
		;r9 = y translation
		;trashes
		;r1-r3, r5-r15

		;save inputs
		vp_sub copy_size, r4
		vp_cpy r1, [r4 + copy_slist]
		vp_cpy r2, [r4 + copy_dlist]
		vp_cpy r3, [r4 + copy_clist]
		vp_cpy r8, [r4 + copy_dx]
		vp_cpy r9, [r4 + copy_dy]

		;run through copy region list
		vp_cpy [r3], r1
		loop_while r1, !=, 0
			vp_cpy [r1 + gui_rect_next], r2
			vp_cpy r2, [r4 + copy_next]
			vp_cpy [r1 + gui_rect_x], r8
			vp_cpy [r1 + gui_rect_y], r9
			vp_cpy [r1 + gui_rect_x1], r10
			vp_cpy [r1 + gui_rect_y1], r11
			vp_cpy [r4 + copy_dx], r12
			vp_cpy [r4 + copy_dy], r13
			vp_add r12, r8
			vp_add r13, r9
			vp_add r12, r10
			vp_add r13, r11
			vp_cpy [r4 + copy_slist], r1
			vp_cpy [r4 + copy_dlist], r2
			static_call region, copy
			vp_cpy [r4 + copy_next], r1
		loop_end
		vp_add copy_size, r4
		vp_ret

	fn_function_end
