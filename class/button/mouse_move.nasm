%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'

	fn_function class/button/mouse_move
		;inputs
		;r0 = button object
		;r1 = mouse event message
		;trashes
		;all but r0, r4

		;hit ?
		m_call button, hit, {r0, [r1 + ev_msg_rx], [r1 + ev_msg_ry]}, {r1}

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
			m_call button, layout, {r0}
			s_jmp button, dirty, {r0}
		endif
		vp_ret

	fn_function_end
