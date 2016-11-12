%include 'inc/func.ninc'
%include 'class/class_view.ninc'

def_func class/view/get_relative
	;inputs
	;r0 = view object
	;r1 = ansestor view object
	;r8 = view x
	;r9 = view y
	;outputs
	;r8 = relative x
	;r9 = relative y
	;trashes
	;r2

	;walk up tree to parent
	vp_cpy r0, r2
	loop_while r2, !=, r1
		vp_add [r2 + view_x], r8
		vp_add [r2 + view_y], r9
		vp_cpy [r2 + view_parent], r2
		assert r2, !=, 0
	loop_end
	vp_ret

def_func_end
