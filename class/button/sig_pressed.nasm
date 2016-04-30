%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'

	fn_function class/button/sig_pressed
		;inputs
		;r0 = button object
		;outputs
		;r1 = pressed signal list
		;trashes
		;all but r0, r4

		vp_lea [r0 + button_pressed_signal], r1
		vp_ret

	fn_function_end
