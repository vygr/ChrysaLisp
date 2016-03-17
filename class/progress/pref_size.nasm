%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/pref_size
		;inputs
		;r0 = progress object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r4

		vp_cpy 8, r10
		vp_cpy 256, r11
		vp_ret

	fn_function_end
