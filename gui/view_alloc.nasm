%include "func.inc"
%include "list.inc"
%include "gui.inc"

	fn_function "gui/view_alloc"
		;outputs
		;r0 = view object
		;trashes
		;r1-r3

		;alloc view objects
		fn_bind gui/gui_statics, r0
		vp_lea [r0 + GUI_STATICS_VIEW_HEAP], r0
		fn_call sys/heap_alloccell
		vp_cpy r1, r0

		;init child list etc
		vp_cpy 0, qword[r0 + GUI_VIEW_PARENT]
		vp_lea [r0 + GUI_VIEW_LIST], r1
		lh_init r1, r2
		vp_cpy 0, qword[r0 + GUI_VIEW_DIRTY_LIST]
		vp_cpy 0, qword[r0 + GUI_VIEW_TRANSPARENT_LIST]

		;init draw callback to null
		vp_lea [rel myret], r1
		vp_cpy r1, [r0 + GUI_VIEW_DRAW]
	myret:
		vp_ret

	fn_function_end
