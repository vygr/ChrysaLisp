%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function class/view/enum_forward
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

		struc enum
			enum_root:	resq 1
			enum_data:	resq 1
			enum_down:	resq 1
			enum_up:	resq 1
		endstruc

		vp_sub enum_size, r4
		vp_cpy r0, [r4 + enum_root]
		vp_cpy r1, [r4 + enum_data]
		vp_cpy r2, [r4 + enum_down]
		vp_cpy r3, [r4 + enum_up]
		vp_cpy r0, r1
		loop_start
		down_loop_ctx:
			vp_cpy r1, r0

			;down callback
			vp_cpy [r4 + enum_data], r1
			vp_call [r4 + enum_down]

			;down to child
			lh_get_tail r0 + view_list, r1
			vp_sub view_node, r1
		loop_until qword[r1 + view_node + ln_node_pred], ==, 0
		loop_start
			;up callback
			vp_cpy [r4 + enum_data], r1
			vp_call [r4 + enum_up]

			;back at root ?
			breakif r0, ==, [r4 + enum_root]

			;across to sibling
			ln_get_pred r0 + view_node, r1
			vp_sub view_node, r1
			jmpif qword[r1 + view_node + ln_node_pred], !=, 0, down_loop_ctx

			;up to parent
			vp_cpy [r0 + view_parent], r0
		loop_end
		vp_add enum_size, r4
		vp_ret

	fn_function_end
