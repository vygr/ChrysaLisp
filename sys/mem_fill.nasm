%include 'inc/func.inc'

	fn_function sys/mem_fill
		;inputs
		;r0 = address
		;r1 = length in bytes
		;r2 = fill byte
		;outputs
		;r0 = address end
		;trashes
		;r1-r3

		if r1, !=, 0
			;not zero length
			vp_cpy r0, r3
			vp_and ptr_size - 1, r3
			if r3, ==, 0
				vp_cpy r1, r3
				vp_and ptr_size - 1, r3
				if r3, ==, 0
					;all aligned on 8 byte boundary
					vp_and 0xff, r2
					vp_cpy r2, r3
					vp_shl 8, r3
					vp_add r3, r2
					vp_shl 8, r3
					vp_add r3, r2
					vp_shl 8, r3
					vp_add r3, r2
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
			loop_start
				vp_cpy_ub r2, [r0]
				vp_inc r0
			loop_until r0, ==, r1
		endif
		vp_ret

	fn_function_end
