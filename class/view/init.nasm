%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/init
		;inputs
		;r0 = object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		super_call view, init
		if r1, !=, 0
			;init myself
			vp_cpy 0, qword[r0 + view_parent]
			vp_lea [r0 + view_list], r1
			lh_init r1, r2
		endif
		vp_ret

	fn_function_end
