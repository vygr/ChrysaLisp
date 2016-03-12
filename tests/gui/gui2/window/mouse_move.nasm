%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'tests/gui/gui2/class_window.inc'

	fn_function tests/gui/gui2/window/mouse_move
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r4

		vp_cpy [r1 + (ml_msg_data + ev_data_x)], r8
		vp_cpy [r1 + (ml_msg_data + ev_data_y)], r9
		vp_sub [r0 + window_last_x], r8
		vp_sub [r0 + window_last_y], r9
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		static_jmp window, move

	fn_function_end
