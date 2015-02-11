%include "func.inc"

	fn_function "sys/enumerate_backwards"
		;inputs
		;r0 = list head
		;r1 = user callback
		;r2 = user data pointer
		;outputs
		;r0 = list head
		;r1 = user callback
		;r2 = user data pointer
		;r5 = status
		;trashes
		;r3, r6
			;callback
			;inputs
			;r0 = list head
			;r1 = user callback
			;r2 = user data pointer
			;r3 = list node
			;outputs
			;r0 = list head
			;r1 = user callback
			;r2 = user data pointer
			;r5 = status
			;can trash
			;r3, r7-r14

		vp_xor r5, r5
		lh_get_tail r0, r6
		repeat
			vp_cpy r6, r3
			ln_get_pred r6, r6
			breakif r6, ==, 0
			vp_call r1
		until r5, !=, 0
		vp_ret

	fn_function_end
