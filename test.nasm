%include "inc/func.ninc"

def_func test

	vp_cpy_i r15, [r0 + 0x7f]
	vp_cpy_i r14, [r1 + 0x7f]
	vp_cpy_i r13, [r2 + 0x7f]
	vp_cpy_i r2, [r13 + 0x7f]
	vp_cpy_i r1, [r14 + 0x7f]
	vp_cpy_i r0, [r15 + 0x7f]

	vp_cpy_i r15, [r0 + 0x7fff]
	vp_cpy_i r14, [r1 + 0x7fff]
	vp_cpy_i r13, [r2 + 0x7fff]
	vp_cpy_i r2, [r13 + 0x7fff]
	vp_cpy_i r1, [r14 + 0x7fff]
	vp_cpy_i r0, [r15 + 0x7fff]

	vp_cpy_i r15, [r0 + 0x7fffffff]
	vp_cpy_i r14, [r1 + 0x7fffffff]
	vp_cpy_i r13, [r2 + 0x7fffffff]
	vp_cpy_i r2, [r13 + 0x7fffffff]
	vp_cpy_i r1, [r14 + 0x7fffffff]
	vp_cpy_i r0, [r15 + 0x7fffffff]

def_func_end
