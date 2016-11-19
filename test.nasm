%include "inc/func.ninc"

def_func test

	vp_mul [rel label1], r15
	vp_mul [rel label1], r14
	vp_mul [rel label1], r13
	vp_mul [rel label1], r2
	vp_mul [rel label1], r1
	vp_mul [rel label1], r0

label1:

def_func_end
