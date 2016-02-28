%include "func.inc"
%include "gui.inc"
%include "heap.inc"

	fn_function "gui/patch_remove"
		;inputs
		;r0 = patch heap pointer
		;r1 = source patch listhead pointer
		;r8 = x (pixels)
		;r9 = y (pixels)
		;r10 = x1 (pixels)
		;r11 = y1 (pixels)
		;trashes
		;r1-r2, r5-r15

		;check for any obvious errors in inputs
		if r10, >, r8
			if r11, >, r9
				;run through source patch list
				vp_cpy r1, r5
				vp_cpy r1, r7
				loop_start
					nextpatch r7, r6

					;not in contact ?
					vp_cpy [r7 + GUI_PATCH_X], r12
					continueif r12, >=, r10
					vp_cpy [r7 + GUI_PATCH_Y], r13
					continueif r13, >=, r11
					vp_cpy [r7 + GUI_PATCH_X1], r14
					continueif r8, >=, r14
					vp_cpy [r7 + GUI_PATCH_Y1], r15
					continueif r9, >=, r15

					;jump to correct splitting code
					jmpif r12, >=, r8, rem_split1
					jmpif r13, >=, r9, rem_split2
					jmpif r10, >=, r14, rem_split4
					jmpif r11, >=, r15, rem_xyx1

				rem_xyx1y1:
					;r8 + r9 + r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + GUI_PATCH_Y]
					;right part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r10, [r1 + GUI_PATCH_X]
					vp_cpy r9, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r11, [r1 + GUI_PATCH_Y1]
					;left part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r9, [r1 + GUI_PATCH_Y]
					vp_cpy r8, [r1 + GUI_PATCH_X1]
					vp_cpy r11, [r1 + GUI_PATCH_Y1]
					;top part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r9, [r1 + GUI_PATCH_Y1]
					continue

				rem_split1:
					;jump to correct splitting code
					jmpif r13, >=, r9, rem_split3
					jmpif r10, >=, r14, rem_split5
					jmpif r11, >=, r15, rem_yx1

				rem_yx1y1:
					;r9 + r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + GUI_PATCH_Y]
					;right part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r10, [r1 + GUI_PATCH_X]
					vp_cpy r9, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r11, [r1 + GUI_PATCH_Y1]
					;top part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r9, [r1 + GUI_PATCH_Y1]
					continue

				rem_split2:
					;jump to correct splitting code
					jmpif r10, >=, r14, rem_split6
					jmpif r11, >=, r15, rem_xx1

				rem_xx1y1:
					;r8 + r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + GUI_PATCH_Y]
					;right part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r10, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r11, [r1 + GUI_PATCH_Y1]
					;left part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r8, [r1 + GUI_PATCH_X1]
					vp_cpy r11, [r1 + GUI_PATCH_Y1]
					continue

				rem_split3:
					;jump to correct splitting code
					jmpif r10, >=, r14, rem_split7
					jmpif r11, >=, r15, rem_x1

				rem_x1y1:
					;r10 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + GUI_PATCH_Y]
					;right part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r10, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r11, [r1 + GUI_PATCH_Y1]
					continue

				rem_split4:
					;jump to correct splitting code
					jmpif r11, >=, r15, rem_xy

				rem_xyy1:
					;r8 + r9 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + GUI_PATCH_Y]
					;left part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r9, [r1 + GUI_PATCH_Y]
					vp_cpy r8, [r1 + GUI_PATCH_X1]
					vp_cpy r11, [r1 + GUI_PATCH_Y1]
					;top part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r9, [r1 + GUI_PATCH_Y1]
					continue

				rem_split5:
					;jump to correct splitting code
					jmpif r11, >=, r15, rem_y

				rem_yy1:
					;r9 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + GUI_PATCH_Y]
					;top part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r9, [r1 + GUI_PATCH_Y1]
					continue

				rem_split6:
					;jump to correct splitting code
					jmpif r11, >=, r15, rem_x

				rem_xy1:
					;r8 + r11 inside
					;bottom part
					vp_cpy r11, [r7 + GUI_PATCH_Y]
					;left part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r8, [r1 + GUI_PATCH_X1]
					vp_cpy r11, [r1 + GUI_PATCH_Y1]
					continue

				rem_split7:
					;jump to correct splitting code
					jmpif r11, >=, r15, rem_encl

				rem_y1:
					;r11 inside
					;bottom part
					vp_cpy r11, [r7 + GUI_PATCH_Y]
					continue

				rem_xyx1:
					;r8 + r9 + r10 inside
					;left part
					vp_cpy r9, [r7 + GUI_PATCH_Y]
					vp_cpy r8, [r7 + GUI_PATCH_X1]
					;right part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r10, [r1 + GUI_PATCH_X]
					vp_cpy r9, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r15, [r1 + GUI_PATCH_Y1]
					;top part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r9, [r1 + GUI_PATCH_Y1]
					continue

				rem_encl:
					;patch is enclosed
					vp_cpy r7, r1
					removepatch r7, r6
					hp_freecell r0, r1, r2
					continue

				rem_x:
					;r8 inside
					;left part
					vp_cpy r8, [r7 + GUI_PATCH_X1]
					continue

				rem_y:
					;r9 inside
					;top part
					vp_cpy r9, [r7 + GUI_PATCH_Y1]
					continue

				rem_xy:
					;r8 + r9 inside
					;left part
					vp_cpy r9, [r7 + GUI_PATCH_Y]
					vp_cpy r8, [r7 + GUI_PATCH_X1]
					;top part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r9, [r1 + GUI_PATCH_Y1]
					continue

				rem_x1:
					;r10 inside
					;right part
					vp_cpy r10, [r7 + GUI_PATCH_X]
					continue

				rem_xx1:
					;r8 + r10 inside
					;right part
					vp_cpy r10, [r7 + GUI_PATCH_X]
					;left part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r8, [r1 + GUI_PATCH_X1]
					vp_cpy r15, [r1 + GUI_PATCH_Y1]
					continue

				rem_yx1:
					;r9 + r10 inside
					;right part
					vp_cpy r10, [r7 + GUI_PATCH_X]
					vp_cpy r9, [r7 + GUI_PATCH_Y]
					;top part
					fn_call sys/heap_alloccell
					continueif r1, ==, 0
					addpatch r5, r1, r2
					vp_cpy r12, [r1 + GUI_PATCH_X]
					vp_cpy r13, [r1 + GUI_PATCH_Y]
					vp_cpy r14, [r1 + GUI_PATCH_X1]
					vp_cpy r9, [r1 + GUI_PATCH_Y1]
				loop_end
			endif
		endif
		vp_ret

	fn_function_end
