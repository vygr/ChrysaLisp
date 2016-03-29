%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_title.inc'

	fn_function class/title/mouse_down
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r0, r4

		vp_cpy [r1 + ev_data_rx], r8
		vp_cpy [r1 + ev_data_ry], r9
		vp_cpy r8, [r0 + title_last_x]
		vp_cpy r9, [r0 + title_last_y]
		vp_ret

	fn_function_end
