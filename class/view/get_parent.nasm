%include 'inc/func.ninc'
%include 'class/class_view.ninc'

def_func class/view/get_parent
	;inputs
	;r0 = view object
	;outputs
	;r0 = view object
	;r1 = parent

	vp_cpy [r0 + view_parent], r1
	vp_ret

def_func_end
