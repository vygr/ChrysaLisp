%include "func.inc"
%include "list.inc"
%include "gui.inc"

	fn_function "gui/view_add"
		;inputs
		;r0 = view object
		;r1 = parent view object
		;trashes
		;r0-r3

		;remove from any existing parent
		vp_cpy r0, r2
		vp_cpy r1, r3
		fn_call gui/view_sub

		;add to parent
		vp_cpy r3, [r2 + GUI_VIEW_PARENT]
		vp_lea [r3 + GUI_VIEW_LIST], r0
		lh_add_at_tail r0, r2, r1
		vp_ret

	fn_function_end
