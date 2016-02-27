%include "func.inc"
%include "list.inc"
%include "gui.inc"

	fn_function "gui/view_sub"
		;inputs
		;r0 = view object
		;trashes
		;r0-r1

		;test parent
		vp_cpy [r0 + GUI_VIEW_PARENT], r1
		if r1, !=, 0
			;clear parent field
			vp_cpy 0, qword[r0 + GUI_VIEW_PARENT]

			;remove from parent list
			ln_remove_node r0, r1
		endif
		vp_ret

	fn_function_end
