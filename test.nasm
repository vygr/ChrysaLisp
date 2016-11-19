%include "inc/func.ninc"

def_func test

	vp_cpy_ui [r0 + 0x7f], r15
	vp_cpy_ui [r1 + 0x7f], r14
	vp_cpy_ui [r2 + 0x7f], r13
	vp_cpy_ui [r13 + 0x7f], r2
	vp_cpy_ui [r14 + 0x7f], r1
	vp_cpy_ui [r15 + 0x7f], r0

	vp_cpy_ui [r0 + 0x7fff], r15
	vp_cpy_ui [r1 + 0x7fff], r14
	vp_cpy_ui [r2 + 0x7fff], r13
	vp_cpy_ui [r13 + 0x7fff], r2
	vp_cpy_ui [r14 + 0x7fff], r1
	vp_cpy_ui [r15 + 0x7fff], r0

	vp_cpy_ui [r0 + 0x7fffffff], r15
	vp_cpy_ui [r1 + 0x7fffffff], r14
	vp_cpy_ui [r2 + 0x7fffffff], r13
	vp_cpy_ui [r13 + 0x7fffffff], r2
	vp_cpy_ui [r14 + 0x7fffffff], r1
	vp_cpy_ui [r15 + 0x7fffffff], r0

def_func_end
