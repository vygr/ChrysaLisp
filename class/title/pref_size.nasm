%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_title.inc'

	fn_function class/title/pref_size
		;inputs
		;r0 = flow object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r4

		vp_cpy 256, r10
		vp_cpy 16, r11
		vp_ret

	fn_function_end
