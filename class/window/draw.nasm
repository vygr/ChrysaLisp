%include 'inc/func.inc'
%include 'class/class_window.inc'

	def_function class/window/draw
		;inputs
		;r0 = window object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		s_jmp window, draw_panel, {r0, r1, 1, window_border_size}

	def_function_end
