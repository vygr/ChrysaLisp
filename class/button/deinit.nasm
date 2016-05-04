%include 'inc/func.inc'
%include 'class/class_button.inc'

	fn_function class/button/deinit
		;inputs
		;r0 = button object
		;trashes
		;all but r0, r4

		;disconnnect all signals
		s_call button, disconnect_sig, {r0, :[r0 + button_pressed_signal]}

		;parent deinit
		p_jmp button, deinit, {r0}

	fn_function_end
