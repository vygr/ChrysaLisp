%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'

	fn_function class/button/draw
		;inputs
		;r0 = button object
		;r1 = ctx object
		;trashes
		;all but r4

		vp_cpy button_border_size, r3
		vp_cpy [r0 + button_state], r2
		vp_and button_state_pressed, r2
		if r2, !=, 0
			vp_mul -1, r3
		endif
		vp_cpy 1, r2
		static_jmp button, draw_panel

	fn_function_end
