%include 'inc/func.ninc'
%include 'class/class_view.ninc'

def_func class/view/set_owner
	;inputs
	;r0 = view object
	;r1 = task tcb

	vp_cpy r1, [r0 + view_tcb]
	vp_ret

def_func_end
