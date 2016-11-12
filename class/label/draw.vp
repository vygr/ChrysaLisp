%include 'inc/func.ninc'
%include 'class/class_label.ninc'

def_func class/label/draw
	;inputs
	;r0 = view object
	;r1 = ctx object
	;trashes
	;all but r0, r4

	;draw panel
	f_jmp label, draw_panel, {r0, r1, 1, label_border_size}

def_func_end
