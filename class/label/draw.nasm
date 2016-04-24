%include 'inc/func.inc'
%include 'class/class_label.inc'

	fn_function class/label/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		;draw panel
		static_jmp label, draw_panel, 'r0, r1, 1, label_border_size'

	fn_function_end
