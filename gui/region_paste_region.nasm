%include 'inc/func.inc'
%include 'inc/gui.inc'

def_func gui/region_paste_region
	;inputs
	;r0 = region heap pointer
	;r1 = source region listhead pointer
	;r2 = dest region listhead pointer
	;r8 = x translation
	;r9 = y translation
	;trashes
	;r1-r3, r5-r15

	def_struc local
		ptr local_node
		ptr local_dlist
		long local_dx
		long local_dy
	def_struc_end

	;save inputs
	vp_sub local_size, r4
	set_src r2, r8, r9
	set_dst [r4 + local_dlist], [r4 + local_dx], [r4 + local_dy]
	map_src_to_dst

	;run through source region list
	loop_flist_forward r1, r1, r1
		vp_cpy r1, [r4 + local_node]

		vp_cpy_i [r1 + gui_rect_x], r8
		vp_cpy_i [r1 + gui_rect_y], r9
		vp_cpy_i [r1 + gui_rect_x1], r10
		vp_cpy_i [r1 + gui_rect_y1], r11
		vp_cpy [r4 + local_dx], r12
		vp_cpy [r4 + local_dy], r13
		vp_add r12, r8
		vp_add r13, r9
		vp_add r12, r10
		vp_add r13, r11
		f_call gui_region, paste_rect, {r0, [r4 + local_dlist], r8, r9, r10, r11}

		vp_cpy [r4 + local_node], r1
	loop_end
	vp_add local_size, r4
	vp_ret

def_func_end
