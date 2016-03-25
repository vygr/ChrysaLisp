%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'

	fn_function class/button/pref_size
		;inputs
		;r0 = button object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r4

		super_call button, pref_size
		vp_add button_border_size * 2, r10
		vp_add button_border_size * 2, r11
		vp_ret

	fn_function_end
