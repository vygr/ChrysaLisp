%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'

	fn_function class/button/mouse_up
		;inputs
		;r0 = button object
		;r1 = mouse event message
		;trashes
		;all but r0, r4

		vp_cpy [r0 + button_state], r1
		vp_push r1

		vp_cpy r1, r2
		vp_and ~button_state_pressed, r1
		vp_cpy r1, [r0 + button_state]
		if r1, !=, r2
			m_call button, layout, {r0}
			s_call button, dirty, {r0}
		endif

		;emit pressed signal ?
		vp_pop r1
		vp_and button_state_pressed, r1
		if r1, !=, 0
			s_jmp button, emit, {r0, :[r0 + button_pressed_signal]}
		endif
		vp_ret

	fn_function_end
