%include 'inc/func.ninc'
%include 'inc/gui.ninc'

def_func gui/region_cut_rect
	;inputs
	;r0 = region heap pointer
	;r1 = source region listhead pointer
	;r2 = dest region listhead pointer
	;r8 = x (pixels)
	;r9 = y (pixels)
	;r10 = x1 (pixels)
	;r11 = y1 (pixels)

	;check for any obvious errors in inputs
	if r10, >, r8
		if r11, >, r9
			;run through source region list
			vp_cpy r2, r5
			vp_cpy r1, r6
			vp_sub ptr_size, r4
			loop_flist_forward r1, r7, [r4]
				;not in contact ?
				vp_cpy_i [r7 + gui_rect_x], r12
				continueif r12, >=, r10
				vp_cpy_i [r7 + gui_rect_y], r13
				continueif r13, >=, r11
				vp_cpy_i [r7 + gui_rect_x1], r14
				continueif r8, >=, r14
				vp_cpy_i [r7 + gui_rect_y1], r15
				continueif r9, >=, r15

				;jump to correct splitting code
				vp_jmpif r12, >=, r8, cut_split1
				vp_jmpif r13, >=, r9, cut_split2
				vp_jmpif r10, >=, r14, cut_split4
				vp_jmpif r11, >=, r15, cut_xyx1

			cut_xyx1y1:
				;r8 + r9 + r10 + r11 inside
				;bottom part
				vp_cpy_i r11, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r8, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r10, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;right part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r10, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;left part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r8, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;top part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r9, [r1 + gui_rect_y1]
				continue

			cut_split1:
				;jump to correct splitting code
				vp_jmpif r13, >=, r9, cut_split3
				vp_jmpif r10, >=, r14, cut_split5
				vp_jmpif r11, >=, r15, cut_yx1

			cut_yx1y1:
				;r9 + r10 + r11 inside
				;bottom part
				vp_cpy_i r11, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r10, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;right part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r10, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;top part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r9, [r1 + gui_rect_y1]
				continue

			cut_split2:
				;jump to correct splitting code
				vp_jmpif r10, >=, r14, cut_split6
				vp_jmpif r11, >=, r15, cut_xx1

			cut_xx1y1:
				;r8 + r10 + r11 inside
				;bottom part
				vp_cpy_i r11, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r8, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r10, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;right part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r10, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;left part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r8, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				continue

			cut_split3:
				;jump to correct splitting code
				vp_jmpif r10, >=, r14, cut_split7
				vp_jmpif r11, >=, r15, cut_x1

			cut_x1y1:
				;r10 + r11 inside
				;bottom part
				vp_cpy_i r11, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r10, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;right part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r10, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				continue

			cut_split4:
				;jump to correct splitting code
				vp_jmpif r11, >=, r15, cut_xy

			cut_xyy1:
				;r8 + r9 + r11 inside
				;bottom part
				vp_cpy_i r11, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r8, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;left part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r8, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;top part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r9, [r1 + gui_rect_y1]
				continue

			cut_split5:
				;jump to correct splitting code
				vp_jmpif r11, >=, r15, cut_y

			cut_yy1:
				;r9 + r11 inside
				;bottom part
				vp_cpy_i r11, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;top part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r9, [r1 + gui_rect_y1]
				continue

			cut_split6:
				;jump to correct splitting code
				vp_jmpif r11, >=, r15, cut_x

			cut_xy1:
				;r8 + r11 inside
				;bottom part
				vp_cpy_i r11, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r8, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				;left part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r8, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				continue

			cut_split7:
				;jump to correct splitting code
				vp_jmpif r11, >=, r15, cut_encl

			cut_y1:
				;r11 inside
				;bottom part
				vp_cpy_i r11, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r11, [r1 + gui_rect_y1]
				continue

			cut_xyx1:
				;r8 + r9 + r10 inside
				;left part
				vp_cpy_i r9, [r7 + gui_rect_y]
				vp_cpy_i r8, [r7 + gui_rect_x1]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r8, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r10, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				;right part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r10, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				;top part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r9, [r1 + gui_rect_y1]
				continue

			cut_encl:
				;region is enclosed
				vp_cpy r7, r1
				vp_cpy [r4], r2
				ln_remove_fnode r7, r2
				ln_add_fnode r5, r1, r2
				continue

			cut_x:
				;r8 inside
				;left part
				vp_cpy_i r8, [r7 + gui_rect_x1]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r8, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				continue

			cut_y:
				;r9 inside
				;top part
				vp_cpy_i r9, [r7 + gui_rect_y1]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				continue

			cut_xy:
				;r8 + r9 inside
				;left part
				vp_cpy_i r9, [r7 + gui_rect_y]
				vp_cpy_i r8, [r7 + gui_rect_x1]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r8, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				;top part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r9, [r1 + gui_rect_y1]
				continue

			cut_x1:
				;r10 inside
				;right part
				vp_cpy_i r10, [r7 + gui_rect_x]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r10, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				continue

			cut_xx1:
				;r8 + r10 inside
				;right part
				vp_cpy_i r10, [r7 + gui_rect_x]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r8, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r10, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				;left part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r8, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				continue

			cut_yx1:
				;r9 + r10 inside
				;right part
				vp_cpy_i r10, [r7 + gui_rect_x]
				vp_cpy_i r9, [r7 + gui_rect_y]
				;cut part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r5, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r9, [r1 + gui_rect_y]
				vp_cpy_i r10, [r1 + gui_rect_x1]
				vp_cpy_i r15, [r1 + gui_rect_y1]
				;top part
				f_call sys_heap, alloc, {r0}, {r1}
				continueif r1, ==, 0
				ln_add_fnode r6, r1, r2
				vp_cpy_i r12, [r1 + gui_rect_x]
				vp_cpy_i r13, [r1 + gui_rect_y]
				vp_cpy_i r14, [r1 + gui_rect_x1]
				vp_cpy_i r9, [r1 + gui_rect_y1]
			loop_end
			vp_add 8, r4
		endif
	endif
	vp_ret

def_func_end
