%include 'inc/func.inc'
%include 'inc/gui.inc'

	def_function gui/region_clip_rect
		;inputs
		;r0 = region heap pointer
		;r1 = source region listhead pointer
		;r8 = x (pixels)
		;r9 = y (pixels)
		;r10 = x1 (pixels)
		;r11 = y1 (pixels)
		;trashes
		;r5-r15

		;check for any obvious errors in inputs
		if r10, >, r8
			if r11, >, r9
				;run through source region list
				vp_cpy r1, r7
				loop_start
				loop:
					ln_next_fnode r7, r6

					switch
						vp_cpy_i [r7 + gui_rect_x], r12
						breakif r12, >=, r10
						vp_cpy_i [r7 + gui_rect_y], r13
						breakif r13, >=, r11
						vp_cpy_i [r7 + gui_rect_x1], r14
						breakif r14, <=, r8
						vp_cpy_i [r7 + gui_rect_y1], r15
						breakif r15, <=, r9

						;clip region
						if r12, <, r8
							vp_cpy_i r8, [r7 + gui_rect_x]
						endif
						if r13, <, r9
							vp_cpy_i r9, [r7 + gui_rect_y]
						endif
						if r14, >, r10
							vp_cpy_i r10, [r7 + gui_rect_x1]
						endif
						if r15, >, r11
							vp_cpy_i r11, [r7 + gui_rect_y1]
						endif
						vp_jmp loop
					endswitch

					;region is outside
					vp_cpy r7, r5
					ln_remove_fnode r7, r6
					hp_freecell r0, r5, r6
				loop_end
			endif
		endif
		vp_ret

	def_function_end
