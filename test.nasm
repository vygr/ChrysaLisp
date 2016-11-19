%include "inc/func.ninc"

def_func test

	vp_cpy_i r15, [r15 + r2 + 1]
	vp_cpy_i r14, [r14 + r1 + 1]
	vp_cpy_i r13, [r13 + r0 + 1]
	vp_cpy_i r2, [r2 + r13 + 1]
	vp_cpy_i r1, [r1 + r14 + 1]
	vp_cpy_i r0, [r0 + r15 + 1]

def_func_end
