%include 'inc/func.ninc'
%include 'inc/font.ninc'
%include 'class/class_title.ninc'

def_func class/title/draw
	;inputs
	;r0 = view object
	;r1 = ctx object
	;trashes
	;all but r0, r4

	;draw panel
	f_jmp title, draw_panel, {r0, r1, 1, title_border_size}

def_func_end
