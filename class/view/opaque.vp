%include 'inc/func.ninc'
%include 'class/class_view.ninc'

def_func class/view/opaque
	;inputs
	;r0 = view object
	;trashes
	;all but r0, r4

	;remove any opaque region
	f_call view, sub_opaque, {r0, -1000000, -1000000, 2000000, 2000000}

	;paste opaque region
	f_jmp view, add_opaque, {r0, 0, 0, [r0 + view_w], [r0 + view_h]}

def_func_end
