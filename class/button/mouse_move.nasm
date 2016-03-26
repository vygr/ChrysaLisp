%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'
%include 'class/class_button.inc'

	fn_function class/button/mouse_move
		;inputs
		;r0 = button object
		;r1 = mouse event message
		;trashes
		;all but r0, r4

		;hit ?
		vp_cpy [r1 + (ml_msg_data + ev_data_rx)], r8
		vp_cpy [r1 + (ml_msg_data + ev_data_ry)], r9
		method_call button, hit

		;is mouse over button ?
		vp_cpy [r0 + button_state], r2
		vp_cpy r2, r3
		if r1, !=, 0
			;state pressed
			vp_or button_state_pressed, r2
		else
			;state not pressed
			vp_and ~button_state_pressed, r2
		endif
		vp_cpy r2, [r0 + button_state]

		;if state changed then dirty
		if r2, !=, r3
			static_call button, dirty
		endif
		vp_ret

	fn_function_end
