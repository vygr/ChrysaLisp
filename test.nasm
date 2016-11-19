%include "inc/func.ninc"

def_func test

	vp_lea [r0 + r15 + 1], r15
	vp_lea [r1 + r14 + 1], r14
	vp_lea [r2 + r13 + 1], r13
	vp_lea [r13 + r2 + 1], r2
	vp_lea [r14 + r1 + 1], r1
	vp_lea [r15 + r0 + 1], r0

	vp_lea [r0 + r15 + 1], r15
	vp_lea [r1 + r14 + 1], r14
	vp_lea [r2 + r13 + 1], r13
	vp_lea [r13 + r2 + 1], r2
	vp_lea [r14 + r1 + 1], r1
	vp_lea [r15 + r0 + 1], r0

	vp_lea [r0 + r15 + 1], r15
	vp_lea [r1 + r14 + 1], r14
	vp_lea [r2 + r13 + 1], r13
	vp_lea [r13 + r2 + 1], r2
	vp_lea [r14 + r1 + 1], r1
	vp_lea [r15 + r0 + 1], r0

def_func_end
