%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function gui/view_add_dirty
		;inputs
		;r0 = view object
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;r0-r3, r5-r15

		;paste dirty patch
		vp_add r8, r10
		vp_add r9, r11
		vp_lea [r0 + gui_view_dirty_list], r1
		fn_bind gui/gui_statics, r0
		vp_lea [r0 + gui_statics_patch_heap], r0
		fn_jmp gui/patch_paste

	fn_function_end
