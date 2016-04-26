%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function class/view/forward_tree
		;inputs
		;r0 = view object
		;r1 = user data pointer
		;r2 = down callback
		;r3 = up callback
		;outputs
		;r0 = view object
		;trashes
		;dependant on callbacks
			;callback api
			;inputs
			;r0 = view object
			;r1 = user data pointer
			;outputs
			;r0 = view object
			;r1 = 0 if should not decend after down callback

		def_local
			def_local_long	inst
			def_local_long	data
			def_local_long	down
			def_local_long	up
		def_local_end

		vp_sub local_size, r4
		vp_cpy r0, .inst
		vp_cpy r1, .data
		vp_cpy r2, .down
		vp_cpy r3, .up
		vp_cpy r0, r1
		loop_start
		down_loop_ctx:
			vp_cpy r1, r0

			;down callback
			vp_cpy .data, r1
			vp_call .down
			breakif r1, ==, 0

			;down to child
			lh_get_tail r0 + view_list, r1
			vp_sub view_node, r1

			ln_get_pred r1 + view_node, r2
		loop_until r2, ==, 0
		loop_start
			;up callback
			vp_cpy .data, r1
			vp_call .up

			;back at root ?
			breakif r0, ==, .inst

			;across to sibling
			ln_get_pred r0 + view_node, r1
			vp_sub view_node, r1

			ln_get_pred r1 + view_node, r2
			jmpif r2, !=, 0, down_loop_ctx

			;up to parent
			vp_cpy [r0 + view_parent], r0
		loop_end

		vp_add local_size, r4
		vp_ret

	fn_function_end
