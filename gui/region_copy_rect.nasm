%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/heap.inc'

	fn_function gui/region_copy_rect
		;inputs
		;r0 = region heap pointer
		;r1 = source region listhead pointer
		;r2 = dest region listhead pointer
		;r8 = x (pixels)
		;r9 = y (pixels)
		;r10 = x1 (pixels)
		;r11 = y1 (pixels)
		;trashes
		;r1-r2, r5-r15

		;check for any obvious errors in inputs
		if r10, >, r8
			if r11, >, r9
				;run through source region list
				vp_cpy r2, r5
				vp_cpy r1, r7
				loop_start
					next_rect r7, r6

					;not in contact ?
					vp_cpy [r7 + gui_rect_x], r12
					continueif r12, >=, r10
					vp_cpy [r7 + gui_rect_y], r13
					continueif r13, >=, r11
					vp_cpy [r7 + gui_rect_x1], r14
					continueif r8, >=, r14
					vp_cpy [r7 + gui_rect_y1], r15
					continueif r9, >=, r15

					static_call sys_heap, alloc
					continueif r1, ==, 0
					add_rect r5, r1, r2

					;jump to correct splitting code
					jmpif r12, >=, r8, copy_split1
					jmpif r13, >=, r9, copy_split2
					jmpif r10, >=, r14, copy_split4
					jmpif r11, >=, r15, copy_xyx1

				copy_xyx1y1:
					;r8 + r9 + r10 + r11 inside
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				copy_split1:
					;jump to correct splitting code
					jmpif r13, >=, r9, copy_split3
					jmpif r10, >=, r14, copy_split5
					jmpif r11, >=, r15, copy_yx1

				copy_yx1y1:
					;r9 + r10 + r11 inside
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				copy_split2:
					;jump to correct splitting code
					jmpif r10, >=, r14, copy_split6
					jmpif r11, >=, r15, copy_xx1

				copy_xx1y1:
					;r8 + r10 + r11 inside
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				copy_split3:
					;jump to correct splitting code
					jmpif r10, >=, r14, copy_split7
					jmpif r11, >=, r15, copy_x1

				copy_x1y1:
					;r10 + r11 inside
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				copy_split4:
					;jump to correct splitting code
					jmpif r11, >=, r15, copy_xy

				copy_xyy1:
					;r8 + r9 + r11 inside
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				copy_split5:
					;jump to correct splitting code
					jmpif r11, >=, r15, copy_y

				copy_yy1:
					;r9 + r11 inside
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				copy_split6:
					;jump to correct splitting code
					jmpif r11, >=, r15, copy_x

				copy_xy1:
					;r8 + r11 inside
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				copy_split7:
					;jump to correct splitting code
					jmpif r11, >=, r15, copy_encl

				copy_y1:
					;r11 inside
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r11, [r1 + gui_rect_y1]
					continue

				copy_xyx1:
					;r8 + r9 + r10 inside
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
					continue

				copy_encl:
					;region is enclosed
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
					continue

				copy_x:
					;r8 inside
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
					continue

				copy_y:
					;r9 inside
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
					continue

				copy_xy:
					;r8 + r9 inside
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r14, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
					continue

				copy_x1:
					;r10 inside
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
					continue

				copy_xx1:
					;r8 + r10 inside
					vp_cpy r8, [r1 + gui_rect_x]
					vp_cpy r13, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
					continue

				copy_yx1:
					;r9 + r10 inside
					vp_cpy r12, [r1 + gui_rect_x]
					vp_cpy r9, [r1 + gui_rect_y]
					vp_cpy r10, [r1 + gui_rect_x1]
					vp_cpy r15, [r1 + gui_rect_y1]
				loop_end
			endif
		endif
		vp_ret

	fn_function_end
