%include 'inc/func.ninc'
%include 'class/class_view.ninc'

def_func class/view/init
	;inputs
	;r0 = object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok

	;init parent
	s_call view, init, {r0, r1}, {r1}
	if r1, !=, 0
		;init myself
		vp_cpy view_flag_solid, r1
		vp_cpy r1, [r0 + view_flags]
		vp_xor r1, r1
		vp_cpy r1, [r0 + view_parent]
		vp_lea [r0 + view_list], r1
		lh_init r1, r2
	endif
	vp_ret

def_func_end
