%include "func.inc"
%include "list.inc"

	fn_function "sys/get_node_at_index"
		;inputs
		;r0 = list head
		;r1 = index
		;outputs
		;r0 = 0, else list node
		;trashes
		;r1, r2

		lh_get_head r0, r2
		loopstart
			vp_cpy r2, r0
			ln_get_succ r2, r2
			if r2, ==, 0
				vp_xor r0, r0
				vp_ret
			endif
			breakif r1, ==, 0
			vp_dec r1
		loopend
		vp_ret

	fn_function_end
