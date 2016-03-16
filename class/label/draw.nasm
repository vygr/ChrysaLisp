%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_label.inc'

	fn_function class/label/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r4

		vp_cpy 1, r2
		vp_xor r3, r3
		static_jmp label, draw_panel

	fn_function_end
