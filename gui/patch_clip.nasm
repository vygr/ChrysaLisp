%include "func.inc"
%include "gui.inc"
%include "heap.inc"

	fn_function "gui/patch_clip"
		;inputs
		;r0 = patch heap pointer
		;r1 = source patch listhead pointer
		;r8 = x (pixels)
		;r9 = y (pixels)
		;r10 = width (pixels)
		;r11 = height (pixels)
		;trashes
		;r1-r3, r5-r15

		;check for any obvious errors in inputs
		if r10, >, 0
			if r11, >, 0
				vp_add r8, r10
				vp_add r9, r11

				;run through source patch list
				vp_cpy r1, r7
				loop_start
				loop:
					nextpatch r7, r6

					switch
					default
						vp_cpy [r7 + GUI_PATCH_X], r12
						breakif r12, >=, r10
						vp_cpy [r7 + GUI_PATCH_Y], r13
						breakif r13, >=, r11
						vp_cpy [r7 + GUI_PATCH_X1], r14
						breakif r14, <=, r8
						vp_cpy [r7 + GUI_PATCH_Y1], r15
						breakif r15, <=, r9

						;clip patch
						if r12, <, r8
							vp_cpy r8, [r7 + GUI_PATCH_X]
						endif
						if r13, <, r9
							vp_cpy r9, [r7 + GUI_PATCH_Y]
						endif
						if r14, >, r10
							vp_cpy r10, [r7 + GUI_PATCH_X1]
						endif
						if r15, >, r11
							vp_cpy r11, [r7 + GUI_PATCH_Y1]
						endif
						vp_jmp loop
					endswitch

					;patch is outside
					vp_cpy r7, r5
					removepatch r7, r6
					hp_freecell r0, r5, r3
				loop_end
			endif
		endif
		vp_ret

	fn_function_end
