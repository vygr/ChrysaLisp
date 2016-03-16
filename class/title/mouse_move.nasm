%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_title.inc'

	fn_function class/title/mouse_move
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r4

		vp_cpy [r1 + (ml_msg_data + ev_data_x)], r12
		vp_cpy [r1 + (ml_msg_data + ev_data_y)], r13
		vp_cpy [r0 + title_last_x], r8
		vp_cpy [r0 + title_last_y], r9
		vp_cpy [r0 + view_parent], r1
		vp_cpy [r1 + view_parent], r1
		static_call title, relative
		vp_cpy r1, r0
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_sub r8, r12
		vp_sub r9, r13
		vp_cpy r12, r8
		vp_cpy r13, r9
		static_jmp view, move

	fn_function_end
