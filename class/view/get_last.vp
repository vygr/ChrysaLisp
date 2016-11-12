%include 'inc/func.ninc'
%include 'class/class_view.ninc'

def_func class/view/get_last
	;inputs
	;r0 = view object
	;outputs
	;r0 = view object
	;r1 = 0 if empty, else last child
	;trashes
	;r2

	lh_get_tail r0 + view_list, r1
	ln_get_pred r1, r2
	if r2, ==, 0
		vp_cpy r2, r1
	else
		vp_sub view_node, r1
	endif
	vp_ret

def_func_end
