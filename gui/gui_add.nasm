%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	def_function gui/gui_add
		;inputs
		;r0 = view object
		;trashes
		;r1-r3

		s_bind gui_gui, statics, r1
		s_jmp view, add_front, {r0, [r1 + gui_statics_screen]}

	def_function_end
