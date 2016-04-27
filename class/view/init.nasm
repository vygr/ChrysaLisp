%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/init
		;inputs
		;r0 = object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		super_call view, init, {r0, r1}, {r1}
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

	fn_function_end
