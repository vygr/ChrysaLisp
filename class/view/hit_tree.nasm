%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/hit_tree
		;inputs
		;r0 = view object
		;r8 = x
		;r9 = y
		;outputs
		;r0 = view object
		;r1 = 0 if not hit, else hit view
		;r8 = x relative to hit
		;r9 = y relative to hit
		;trashes
		;r1-r3

		def_local
			def_local_long	inst
			def_local_long	hit
			def_local_long	jump
		def_local_end

		;save inputs
		vp_sub	local_size, r4
		vp_cpy r0, .inst
		vp_cpy_cl 0, .hit
		vp_rel early_ret, r1
		vp_cpy r1, .jump

		;iterate through views front to back
		static_call view, forward_tree, {r0, r4, $hit_down_callback, $hit_up_callback}

	early_ret:
		vp_cpy .inst, r0
		vp_cpy .hit, r1
		vp_add local_size, r4
		vp_ret

	hit_down_callback:
		vp_sub [r0 + view_x], r8
		vp_sub [r0 + view_y], r9
		vp_cpy [r0 + view_flags], r1
		vp_and view_flag_solid, r1
		vp_ret

	hit_up_callback:
		vp_cpy r1, r2
		method_call view, hit, {r0, r8, r9}, {r1}
		if r1, !=, 0
			;early exit back to caller !
			vp_cpy r2, r4
			vp_cpy r0, [r2 + local_hit]
			vp_jmp [r2 + local_jump]
		endif
		vp_add [r0 + view_x], r8
		vp_add [r0 + view_y], r9
		vp_ret

	fn_function_end
