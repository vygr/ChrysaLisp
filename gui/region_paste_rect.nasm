%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function gui/region_paste_rect
		;inputs
		;r0 = region heap pointer
		;r1 = dest region listhead pointer
		;r8 = x (pixels)
		;r9 = y (pixels)
		;r10 = x1 (pixels)
		;r11 = y1 (pixels)
		;trashes
		;r1-r3, r5-r15

		;check for any obvious errors in inputs
		if r10, >, r8
			if r11, >, r9
				;run through source region list
				vp_cpy r1, r5
				loop_flist_forward r1, r7, r6
					;not in contact ?
					vp_cpy [r7 + gui_rect_x], r12
					continueif r12, >=, r10
					vp_cpy [r7 + gui_rect_y], r13
					continueif r13, >=, r11
					vp_cpy [r7 + gui_rect_x1], r14
					continueif r8, >=, r14
					vp_cpy [r7 + gui_rect_y1], r15
					continueif r9, >=, r15

					;jump to correct splitting code
					jmpif r12, >=, r8, paste_split1
					jmpif r13, >=, r9, paste_split2
					jmpif r10, >=, r14, paste_split4
					jmpif r11, >=, r15, paste_xyx1

				paste_xyx1y1:
					;r8 + r9 + r10 + r11 inside
					vp_ret

				paste_split1:
					;jump to correct splitting code
					jmpif r13, >=, r9, paste_split3
					jmpif r10, >=, r14, paste_split5
					jmpif r11, >=, r15, paste_yx1

				paste_yx1y1:
					;r9 + r10 + r11 inside
					vp_cpy r12, r10
					continue

				paste_split2:
					;jump to correct splitting code
					jmpif r10, >=, r14, paste_split6
					jmpif r11, >=, r15, paste_xx1

				paste_xx1y1:
					;r8 + r10 + r11 inside
					vp_cpy r13, r11
					continue

				paste_split3:
					;jump to correct splitting code
					jmpif r10, >=, r14, paste_split7
					jmpif r11, >=, r15, paste_x1

				paste_x1y1:
					;r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_rect_y]
					;right part
					static_call sys_heap, alloc, {r0}, {r1}
					continueif r1, ==, 0
					ln_add_fnode r5, r1, r2
					vp_cpy r10, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				paste_split4:
					;jump to correct splitting code
					jmpif r11, >=, r15, paste_xy

				paste_xyy1:
					;r8 + r9 + r11 inside
					vp_cpy r14, r8
					continue

				paste_split5:
					;jump to correct splitting code
					jmpif r11, >=, r15, paste_y

				paste_yy1:
					;r9 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_rect_y]
					;top part
					static_call sys_heap, alloc, {r0}, {r1}
					continueif r1, ==, 0
					ln_add_fnode r5, r1, r2
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r9, [r1 + gui_rect_y1]
					continue

				paste_split6:
					;jump to correct splitting code
					jmpif r11, >=, r15, paste_x

				paste_xy1:
					;r8 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_rect_y]
					;left part
					static_call sys_heap, alloc, {r0}, {r1}
					continueif r1, ==, 0
					ln_add_fnode r5, r1, r2
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r8, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				paste_split7:
					;jump to correct splitting code
					jmpif r11, >=, r15, paste_encl

				paste_y1:
					;r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_rect_y]
					continue

				paste_xyx1:
					;r8 + r9 + r10 inside
					vp_cpy r15, r9
					continue

				paste_encl:
					;region is enclosed
					vp_cpy r7, r1
					ln_remove_fnode r7, r6
					hp_freecell r0, r1, r2
					continue

				paste_x:
					;r8 inside
					;left part
					vp_cpy r8, [r7 + gui_rect_x1]
					continue

				paste_y:
					;r9 inside
					;top part
					vp_cpy r9, [r7 + gui_rect_y1]
					continue

				paste_xy:
					;r8 + r9 inside
					;left part
					vp_cpy r9, [r7 + gui_rect_y]
					vp_cpy r8, [r7 + gui_rect_x1]
					;top part
					static_call sys_heap, alloc, {r0}, {r1}
					continueif r1, ==, 0
					ln_add_fnode r5, r1, r2
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r9, [r1 + gui_rect_y1]
					continue

				paste_x1:
					;r10 inside
					;right part
					vp_cpy r10, [r7 + gui_rect_x]
					continue

				paste_xx1:
					;r8 + r10 inside
					;right part
					vp_cpy r10, [r7 + gui_rect_x]
					;left part
					static_call sys_heap, alloc, {r0}, {r1}
					continueif r1, ==, 0
					ln_add_fnode r5, r1, r2
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r8, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
					continue

				paste_yx1:
					;r9 + r10 inside
					;right part
					vp_cpy r10, [r7 + gui_rect_x]
					vp_cpy r9, [r7 + gui_rect_y]
					;top part
					static_call sys_heap, alloc, {r0}, {r1}
					continueif r1, ==, 0
					ln_add_fnode r5, r1, r2
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r9, [r1 + gui_rect_y1]
				loop_end

				;create new region ?
				switch
					breakif r8, ==, r10
					breakif r9, ==, r11
					static_call sys_heap, alloc, {r0}, {r1}
					breakif r1, ==, 0

					ln_add_fnode r5, r1, r2
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
				endswitch
			endif
		endif
		vp_ret

	fn_function_end
