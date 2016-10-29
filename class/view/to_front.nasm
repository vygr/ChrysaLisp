%include 'inc/func.inc'
%include 'class/class_view.inc'

def_func class/view/to_front
	;inputs
	;r0 = view object
	;outputs
	;r0 = view object
	;trashes
	;r2

	;are we allready front ?
	ln_is_first r0 + view_node, r1
	if r1, !=, 0
		f_call view, add_front, {r0, [r0 + view_parent]}
		f_jmp view, dirty_all, {r0}
	endif
	vp_ret

def_func_end
