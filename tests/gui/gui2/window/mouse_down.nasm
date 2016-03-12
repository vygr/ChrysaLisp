%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'tests/gui/gui2/class_window.inc'

	fn_function tests/gui/gui2/window/mouse_down
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r4

		vp_cpy [r1 + (ml_msg_data + ev_data_rx)], r8
		vp_cpy [r1 + (ml_msg_data + ev_data_ry)], r9
		vp_cpy r8, [r0 + window_last_x]
		vp_cpy r9, [r0 + window_last_y]
		vp_ret

	fn_function_end
