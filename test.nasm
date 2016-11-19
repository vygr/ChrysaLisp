%include "inc/func.ninc"

def_func test

	vp_cpy_ub [r15 + r2 + 1], r15
	vp_cpy_ub [r14 + r1 + 1], r14
	vp_cpy_ub [r13 + r0 + 1], r13
	vp_cpy_ub [r2 + r13 + 1], r2
	vp_cpy_ub [r1 + r14 + 1], r1
	vp_cpy_ub [r0 + r15 + 1], r0

def_func_end
