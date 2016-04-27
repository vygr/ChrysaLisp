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
		;all but r0, r4

		super_call title, pref_size, {r0}, {r10, r11}
		vp_add title_border_size * 2, r10
		vp_add title_border_size * 2, r11
		vp_ret

	fn_function_end
