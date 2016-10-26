%include 'inc/func.inc'

def_func sys/string_hash
	;inputs
	;r0 = string
	;outputs
	;r1 = string hash
	;trashes
	;r0, r2

	vp_cpy 5381, r1
	loop_start
		vp_cpy_ub [r0], r2
		breakif r2, ==, 0
		vp_inc r0
		vp_mul 33, r1
		vp_add r2, r1
	loop_end
	vp_ret

def_func_end
