%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_title.inc'

	fn_function class/title/mouse_move
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r4

		vp_cpy [r1 + (ml_msg_data + ev_data_x)], r8
		vp_cpy [r1 + (ml_msg_data + ev_data_y)], r9
		vp_sub [r0 + title_last_x], r8
		vp_sub [r0 + title_last_y], r9
		vp_sub [r0 + view_x], r8
		vp_sub [r0 + view_y], r9
		vp_cpy [r0 + view_parent], r0
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		static_jmp view, move

	fn_function_end
