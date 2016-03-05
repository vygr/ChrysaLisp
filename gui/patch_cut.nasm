%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/heap.inc'

	fn_function gui/patch_cut
		;inputs
		;r0 = patch heap pointer
		;r1 = source patch listhead pointer
		;r2 = dest patch listhead pointer
		;r8 = x (pixels)
		;r9 = y (pixels)
		;r10 = x1 (pixels)
		;r11 = y1 (pixels)

		;check for any obvious errors in inputs
		if r10, >, r8
			if r11, >, r9
				;run through source patch list
				vp_cpy r2, r5
				vp_cpy r1, r6
				vp_cpy r1, r7
				vp_sub 8, r4
				loop_start
					nextpatch r7, [r4]

					;not in contact ?
					vp_cpy [r7 + gui_patch_x], r12
					continueif r12, >=, r10
					vp_cpy [r7 + gui_patch_y], r13
					continueif r13, >=, r11
					vp_cpy [r7 + gui_patch_x1], r14
					continueif r8, >=, r14
					vp_cpy [r7 + gui_patch_y1], r15
					continueif r9, >=, r15

					;jump to correct splitting code
					jmpif r12, >=, r8, cut_split1
					jmpif r13, >=, r9, cut_split2
					jmpif r10, >=, r14, cut_split4
					jmpif r11, >=, r15, cut_xyx1

				cut_xyx1y1:
					;r8 + r9 + r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r8, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r10, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;right part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r10, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;left part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r8, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;top part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r9, [r1 + gui_patch_y1]
					continue

				cut_split1:
					;jump to correct splitting code
					jmpif r13, >=, r9, cut_split3
					jmpif r10, >=, r14, cut_split5
					jmpif r11, >=, r15, cut_yx1

				cut_yx1y1:
					;r9 + r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r10, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;right part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r10, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;top part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r9, [r1 + gui_patch_y1]
					continue

				cut_split2:
					;jump to correct splitting code
					jmpif r10, >=, r14, cut_split6
					jmpif r11, >=, r15, cut_xx1

				cut_xx1y1:
					;r8 + r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r8, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r10, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;right part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r10, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;left part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r8, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					continue

				cut_split3:
					;jump to correct splitting code
					jmpif r10, >=, r14, cut_split7
					jmpif r11, >=, r15, cut_x1

				cut_x1y1:
					;r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r10, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;right part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r10, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					continue

				cut_split4:
					;jump to correct splitting code
					jmpif r11, >=, r15, cut_xy

				cut_xyy1:
					;r8 + r9 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r8, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;left part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r8, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;top part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r9, [r1 + gui_patch_y1]
					continue

				cut_split5:
					;jump to correct splitting code
					jmpif r11, >=, r15, cut_y

				cut_yy1:
					;r9 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;top part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r9, [r1 + gui_patch_y1]
					continue

				cut_split6:
					;jump to correct splitting code
					jmpif r11, >=, r15, cut_x

				cut_xy1:
					;r8 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r8, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					;left part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r8, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					continue

				cut_split7:
					;jump to correct splitting code
					jmpif r11, >=, r15, cut_encl

				cut_y1:
					;r11 inside
					;bottom part
					vp_cpy r11, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r11, [r1 + gui_patch_y1]
					continue

				cut_xyx1:
					;r8 + r9 + r10 inside
					;left part
					vp_cpy r9, [r7 + gui_patch_y]
					vp_cpy r8, [r7 + gui_patch_x1]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r8, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r10, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					;right part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r10, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					;top part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r9, [r1 + gui_patch_y1]
					continue

				cut_encl:
					;patch is enclosed
					vp_cpy r7, r1
					vp_cpy [r4], r2
					removepatch r7, r2
					addpatch r5, r1, r2
					continue

				cut_x:
					;r8 inside
					;left part
					vp_cpy r8, [r7 + gui_patch_x1]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r8, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					continue

				cut_y:
					;r9 inside
					;top part
					vp_cpy r9, [r7 + gui_patch_y1]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					continue

				cut_xy:
					;r8 + r9 inside
					;left part
					vp_cpy r9, [r7 + gui_patch_y]
					vp_cpy r8, [r7 + gui_patch_x1]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r8, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					;top part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r9, [r1 + gui_patch_y1]
					continue

				cut_x1:
					;r10 inside
					;right part
					vp_cpy r10, [r7 + gui_patch_x]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r10, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					continue

				cut_xx1:
					;r8 + r10 inside
					;right part
					vp_cpy r10, [r7 + gui_patch_x]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r8, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r10, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					;left part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r8, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					continue

				cut_yx1:
					;r9 + r10 inside
					;right part
					vp_cpy r10, [r7 + gui_patch_x]
					vp_cpy r9, [r7 + gui_patch_y]
					;cut part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r9, [r1 + gui_patch_y]
					vp_cpy r10, [r1 + gui_patch_x1]
					vp_cpy r15, [r1 + gui_patch_y1]
					;top part
					class_call heap, alloc
					continueif r1, ==, 0
					addpatch r6, r1, r2
					vp_cpy r12, [r1 + gui_patch_x]
					vp_cpy r13, [r1 + gui_patch_y]
					vp_cpy r14, [r1 + gui_patch_x1]
					vp_cpy r9, [r1 + gui_patch_y1]
				loop_end
				vp_add 8, r4
			endif
		endif
		vp_ret

	fn_function_end
