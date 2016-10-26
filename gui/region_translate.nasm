%include 'inc/func.inc'
%include 'inc/gui.inc'

	def_func gui/region_translate
		;inputs
		;r1 = region listhead pointer
		;r8 = x translation
		;r9 = y translation
		;trashes
		;r1, r10, r11

		;run through source region list
		loop_flist_forward r1, r1, r1
			vp_cpy_i [r1 + gui_rect_x], r10
			vp_cpy_i [r1 + gui_rect_y], r11
			vp_add r8, r10
			vp_add r9, r11
			vp_cpy_i r10, [r1 + gui_rect_x]
			vp_cpy_i r11, [r1 + gui_rect_y]
			vp_cpy_i [r1 + gui_rect_x1], r10
			vp_cpy_i [r1 + gui_rect_y1], r11
			vp_add r8, r10
			vp_add r9, r11
			vp_cpy_i r10, [r1 + gui_rect_x1]
			vp_cpy_i r11, [r1 + gui_rect_y1]
		loop_end
		vp_ret

	def_func_end
