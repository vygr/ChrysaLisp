%include 'inc/func.inc'

def_func sys/mem_copy
	;inputs
	;r0 = source address
	;r1 = destination address
	;r2 = length in bytes
	;outputs
	;r0 = source address end
	;r1 = destination address end
	;trashes
	;r2-r3

	if r2, !=, 0
		;not zero length
		vp_cpy r0, r3
		vp_and ptr_size - 1, r3
		if r3, ==, 0
			vp_cpy r1, r3
			vp_and ptr_size - 1, r3
			if r3, ==, 0
				vp_cpy r2, r3
				vp_and ptr_size - 1, r3
				if r3, ==, 0
					;all aligned on 8 byte boundary
					vp_add r0, r2
					loop_start
						vp_cpy [r0], r3
						vp_cpy r3, [r1]
						vp_add ptr_size, r0
						vp_add ptr_size, r1
					loop_until r0, ==, r2
					vp_ret
				endif
			endif
		endif
		;something not aligned so byte copy
		vp_add r0, r2
		loop_start
			vp_cpy_ub [r0], r3
			vp_cpy_ub r3, [r1]
			vp_inc r0
			vp_inc r1
		loop_until r0, ==, r2
	endif
	vp_ret

def_func_end
