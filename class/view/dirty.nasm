%include 'inc/func.inc'
%include 'class/class_view.inc'

def_func class/view/dirty
	;inputs
	;r0 = view object
	;trashes
	;all but r0, r4

	;paste dirty region
	f_jmp view, add_dirty, {r0, 0, 0, [r0 + view_w], [r0 + view_h]}

def_func_end
