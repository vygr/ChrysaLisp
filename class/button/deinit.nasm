%include 'inc/func.inc'
%include 'class/class_button.inc'

	fn_function class/button/deinit
		;inputs
		;r0 = button object
		;trashes
		;all but r0, r4

		;disconnnect all signals
		vp_lea [r0 + button_pressed_signal], r1
		static_call button, disconnect_sig

		;parent deinit
		super_jmp button, deinit

	fn_function_end
