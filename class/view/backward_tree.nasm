%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	def_func class/view/backward_tree
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

		def_struc local
			ptr local_inst
			ptr local_data
			ptr local_down
			ptr local_up
		def_struc_end

		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]
		vp_cpy r1, [r4 + local_data]
		vp_cpy r2, [r4 + local_down]
		vp_cpy r3, [r4 + local_up]
		vp_cpy r0, r1
		loop_start
		down_loop_ctx:
			vp_cpy r1, r0

			;down callback
			vp_cpy [r4 + local_data], r1
			vp_call [r4 + local_down]
			breakif r1, ==, 0

			;down to child
			lh_get_tail r0 + view_list, r1
			vp_sub view_node, r1

			ln_get_pred r1 + view_node, r2
		loop_until r2, ==, 0
		loop_start
			;up callback
			vp_cpy [r4 + local_data], r1
			vp_call [r4 + local_up]

			;back at root ?
			breakif r0, ==, [r4 + local_inst]

			;across to sibling
			ln_get_pred r0 + view_node, r1
			vp_sub view_node, r1

			ln_get_pred r1 + view_node, r2
			vp_jmpif r2, !=, 0, down_loop_ctx

			;up to parent
			vp_cpy [r0 + view_parent], r0
		loop_end

		vp_add local_size, r4
		vp_ret

	def_func_end
