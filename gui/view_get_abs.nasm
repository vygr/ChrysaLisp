%include 'inc/func.inc'
%include 'inc/list.inc'
%include 'inc/gui.inc'

	fn_function gui/view_get_abs
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
		loop_while qword[r0 + gui_view_parent], !=, 0
			vp_add [r0 + gui_view_x], r8
			vp_add [r0 + gui_view_y], r9
			vp_cpy [r0 + gui_view_parent], r0
		loop_end
		vp_ret

	fn_function_end
