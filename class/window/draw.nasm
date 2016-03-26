%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'

	fn_function class/window/draw
		;inputs
		;r0 = window object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		vp_xor r2, r2
		vp_cpy window_border_size, r3
		static_jmp window, draw_panel

	fn_function_end
