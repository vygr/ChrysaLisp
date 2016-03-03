%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function gui/view_add_transparent
		;inputs
		;r0 = view object
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;r0-r3, r5-r15

		;paste transparent patch
		vp_cpy r8, r12
		vp_cpy r9, r13
		vp_lea [r0 + GUI_VIEW_TRANSPARENT_LIST], r1
		fn_call gui/view_get_abs
		vp_add r12, r8
		vp_add r13, r9
		vp_add r8, r10
		vp_add r9, r11
		fn_bind gui/gui_statics, r0
		vp_lea [r0 + gui_statics_patch_heap], r0
		fn_jmp gui/patch_paste

	fn_function_end
