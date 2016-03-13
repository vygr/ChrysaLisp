%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'

	fn_function class/button/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r4

		vp_cpy 1, r2
		static_call button, draw_panel
		vp_ret

	fn_function_end
