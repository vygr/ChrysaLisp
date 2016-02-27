%include "func.inc"
%include "gui.inc"

	fn_function "gui/view_update"
		;inputs
		;r0 = view object
		;trashes
		;r0-r3, r5-r15

		;paste dirty patch
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r0 + GUI_VIEW_W], r10
		vp_cpy [r0 + GUI_VIEW_H], r11
		vp_lea [r0 + GUI_VIEW_DIRTY_LIST], r1
		fn_bind gui/gui_statics, r0
		vp_lea [r0 + GUI_STATICS_PATCH_HEAP], r0
		fn_jmp gui/patch_paste

	fn_function_end
