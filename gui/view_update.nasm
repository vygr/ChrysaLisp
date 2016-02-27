%include "func.inc"
%include "list.inc"
%include "gui.inc"

	fn_function "gui/view_update"
		;inputs
		;r0 = view object
		;trashes
		;r0-r3, r5

		;save view
		vp_cpy r0, r5

		;allocate dirty patch
		fn_bind gui/gui_statics, r0
		vp_add GUI_STATICS_PATCH_HEAP, r0
		fn_call sys/heap_alloccell

		;add to view dirty list
		vp_cpy [r5 + GUI_VIEW_W], r2
		vp_cpy [r5 + GUI_VIEW_H], r3
		vp_cpy 0, qword[r1 + GUI_PATCH_X]
		vp_cpy 0, qword[r1 + GUI_PATCH_Y]
		vp_cpy r2, [r1 + GUI_PATCH_W]
		vp_cpy r3, [r1 + GUI_PATCH_H]
		vp_add GUI_VIEW_DIRTY_LIST, r5
		lh_add_at_tail r5, r1, r2
		vp_ret

	fn_function_end
