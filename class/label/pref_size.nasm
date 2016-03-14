%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_label.inc'

	fn_function class/label/pref_size
		;inputs
		;r0 = label object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r4

		vp_cpy 64, r10
		vp_cpy 32, r11
		vp_ret

	fn_function_end
