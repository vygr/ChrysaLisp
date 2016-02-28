%include "func.inc"
%include "list.inc"
%include "gui.inc"

	fn_function "gui/view_get_abs"
		;inputs
		;r0 = view object
		;outputs
		;r8 = abs x
		;r9 = abs y
		;trashes
		;r0

		;walk up tree to parent
		vp_xor r8, r8
		vp_xor r9, r9
		loop_while qword[r0 + GUI_VIEW_PARENT], !=, 0
			vp_add [r0 + GUI_VIEW_X], r8
			vp_add [r0 + GUI_VIEW_Y], r9
			vp_cpy [r0 + GUI_VIEW_PARENT], r0
		loop_end
		vp_ret

	fn_function_end
