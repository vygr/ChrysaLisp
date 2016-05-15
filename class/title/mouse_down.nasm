%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_title.inc'
%include 'class/class_window.inc'

	fn_function class/title/mouse_down
		;inputs
		;r0 = title object
		;r1 = mouse event message
		;trashes
		;all but r0, r4

		vp_push r0
		vp_cpy [r1 + ev_data_rx], r8
		vp_cpy [r1 + ev_data_ry], r9
		vp_cpy r8, [r0 + title_last_x]
		vp_cpy r9, [r0 + title_last_y]
		vp_cpy [r0 + view_parent], r0
		vp_cpy [r0 + view_parent], r0
		s_call window, to_front, {r0}
		vp_pop r0
		vp_ret

	fn_function_end
