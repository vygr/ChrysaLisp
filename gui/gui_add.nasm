%include 'inc/func.ninc'
%include 'inc/gui.ninc'
%include 'class/class_view.ninc'

def_func gui/gui_add
	;inputs
	;r0 = view object
	;trashes
	;r1-r3

	f_bind gui_gui, statics, r1
	f_jmp view, add_front, {r0, [r1 + gui_statics_screen]}

def_func_end
