%include 'inc/func.inc'
%include 'inc/list.inc'

	fn_function sys/list_get_index_of_node
		;inputs
		;r0 = list head
		;r1 = list node
		;outputs
		;r0 = -1, else index
		;r1 = list node
		;trashes
		;r2, r3

		lh_get_head r0, r2
		vp_xor r0, r0
		loop_start
			vp_cpy r2, r3
			ln_get_succ r2, r2
			if r2, ==, 0
				vp_cpy -1, r0
				vp_ret
			endif
			breakif r3, ==, r1
			vp_inc r0
		loop_end
		vp_ret

	fn_function_end
