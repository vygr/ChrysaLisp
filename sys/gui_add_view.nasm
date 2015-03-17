%include "func.inc"
%include "list.inc"
%include "gui.inc"

	fn_function "sys/gui_add_view"
		;inputs
		;r0 = view object
		;r1 = parent view object
		;outputs
		;r0 = view object
		;trashes
		;r1-r3

		;remove from any existing parent
		vp_cpy r1, r3
		fn_call sys/gui_sub_view

		;add to parent
		vp_cpy r3, [r0 + GUI_VIEW_PARENT]
		vp_lea [r3 + GUI_VIEW_LIST], r1
		lh_add_at_head r1, r0, r2
		vp_ret

	fn_function_end
