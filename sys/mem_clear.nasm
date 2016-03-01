%include "inc/func.inc"

	fn_function "sys/mem_clear"
		;inputs
		;r0 = address
		;r1 = length in bytes
		;trashes
		;r0-r2

		if r1, !=, 0
			;not zero length
			vp_cpy r0, r2
			vp_and 7, r2
			if r2, ==, 0
				vp_cpy r1, r2
				vp_and 7, r2
				if r2, ==, 0
					;all aligned on 8 byte boundary
					vp_add r0, r1
					loop_start
						vp_cpy r2, [r0]
						vp_add 8, r0
					loop_until r0, ==, r1
					vp_ret
				endif
			endif
			;something not aligned
			vp_add r0, r1
			vp_xor r2, r2
			loop_start
				vp_cpy r2l, byte[r0]
				vp_inc r0
			loop_until r0, ==, r1
		endif
		vp_ret

	fn_function_end
