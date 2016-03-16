%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'

	fn_function class/button/draw
		;inputs
		;r0 = window object
		;r1 = ctx object
		;trashes
		;all but r4

		vp_cpy 1, r2
		vp_cpy button_border_size, r3
		static_jmp button, draw_panel

	fn_function_end
