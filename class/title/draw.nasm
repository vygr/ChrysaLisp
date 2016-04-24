%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/font.inc'
%include 'class/class_title.inc'

	fn_function class/title/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		;draw panel
		static_jmp title, draw_panel, 'r0, r1, 1, title_border_size'

	fn_function_end
