%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'

	fn_function class/button/mouse_down
		;inputs
		;r0 = button object
		;r1 = mouse event message
		;trashes
		;all but r4

		vp_cpy [r0 + button_state], r1
		vp_or button_state_pressed, r1
		vp_cpy r1, [r0 + button_state]
		static_jmp button, dirty

	fn_function_end
