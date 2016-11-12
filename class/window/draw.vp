%include 'inc/func.ninc'
%include 'class/class_window.ninc'

def_func class/window/draw
	;inputs
	;r0 = window object
	;r1 = ctx object
	;trashes
	;all but r0, r4

	f_jmp window, draw_panel, {r0, r1, 1, window_border_size}

def_func_end
