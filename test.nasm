%include "inc/func.ninc"

def_func test

	vp_cpy r15, [rel label1]
	vp_cpy r14, [rel label1]
	vp_cpy r13, [rel label1]
	vp_cpy r2, [rel label1]
	vp_cpy r1, [rel label1]
	vp_cpy r0, [rel label1]

label1:

def_func_end
