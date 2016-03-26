%include 'inc/func.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'

	fn_function class/window/pref_size
		;inputs
		;r0 = window object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r4

		vp_cpy [r0 + window_flow], r0
		method_call flow, pref_size
		vp_add window_border_size * 2, r10
		vp_add window_border_size * 2, r11
		vp_ret

	fn_function_end
