%include "func.inc"
%include "gui.inc"
%include "heap.inc"

	fn_function "gui/patch_free"
		;inputs
		;r0 = patch heap pointer
		;r1 = source patch listhead pointer
		;trashes
		;r1-r3

		;run through source patch list
		vp_cpy [r1], r2
		vp_cpy 0, qword[r1]
		loop_while r2, !=, 0
			vp_cpy r2, r1
			vp_cpy [r2 + GUI_PATCH_NEXT], r2
			hp_freecell r0, r1, r3
		loop_end
		vp_ret

	fn_function_end
