%include "func.inc"
%include "list.inc"
%include "gui.inc"

	fn_function "gui/view_sub"
		;inputs
		;r0 = view object
		;outputs
		;r0 = view object
		;trashes
		;r1-r2

		;test parent
		vp_cpy [r0 + GUI_VIEW_PARENT], r1
		if r1, !=, 0
			;remove from parent list
			vp_cpy r0, r1
			ln_remove_node r1, r2

			;clear parent field
			vp_cpy 0, qword[r0 + GUI_VIEW_PARENT]
		endif
		vp_ret

	fn_function_end
