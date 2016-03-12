%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/hit_tree
		;inputs
		;r0 = view object root
		;r8 = x
		;r9 = y
		;inputs
		;r0 = 0, else view object hit
		;r8 = x relative to hit
		;r9 = y relative to hit
		;trashes
		;r1-r3

		;iterate through views front to back
		vp_cpy r4, r1
		vp_lea [rel hit_down_callback], r2
		vp_lea [rel hit_up_callback], r3
		static_call view, forward_tree
		vp_xor r0, r0
		vp_ret

	hit_down_callback:
		vp_sub [r0 + view_x], r8
		vp_sub [r0 + view_y], r9
		vp_ret

	hit_up_callback:
		vp_cpy r0, r2
		method_call view, hit
		if r0, !=, 0
			;early exit back to caller !
			vp_cpy r1, r4
			vp_ret
		endif
		vp_cpy r2, r0
		vp_add [r0 + view_x], r8
		vp_add [r0 + view_y], r9
		vp_ret

	fn_function_end
