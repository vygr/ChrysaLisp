%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'

	fn_function class/window/mouse_down
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r4

		;set drag code and offset
		vp_cpy [r1 + (ml_msg_data + ev_data_rx)], r8
		vp_cpy [r1 + (ml_msg_data + ev_data_ry)], r9
		vp_cpy r8, r12
		vp_cpy r9, r13
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_sub window_border_size * 2, r10
		vp_sub window_border_size * 2, r11
		vp_xor r15, r15
		if r8, <, window_border_size * 2
			;left edge
			vp_or window_drag_left, r15
		endif
		if r8, >=, r10
			;right edge
			vp_or window_drag_right, r15
			vp_sub [r0 + view_w], r12
		endif
		if r9, <, window_border_size * 2
			;top edge
			vp_or window_drag_top, r15
		endif
		if r9, >=, r11
			;bottom edge
			vp_or window_drag_bottom, r15
			vp_sub [r0 + view_h], r13
		endif
		vp_cpy r15, [r0 + window_drag_mode]
		vp_cpy r12, [r0 + window_offset_x]
		vp_cpy r13, [r0 + window_offset_y]
		vp_ret

	fn_function_end
