%include "inc/func.ninc"

def_func test

vp_mul [r0], r0
vp_mul [r0 + 0x7f], r0
vp_mul [r0 + 0x7fff], r0

vp_mul [r1], r0
vp_mul [r1 + 0x7f], r0
vp_mul [r1 + 0x7fff], r0

vp_mul [r15], r0
vp_mul [r15 + 0x7f], r0
vp_mul [r15 + 0x7fff], r0

vp_mul [r4], r0
vp_mul [r4 + 0x7f], r0
vp_mul [r4 + 0x7fff], r0

vp_mul [r12], r0
vp_mul [r12 + 0x7f], r0
vp_mul [r12 + 0x7fff], r0

vp_mul [r0], r10
vp_mul [r0 + 0x7f], r10
vp_mul [r0 + 0x7fff], r10

vp_mul [r1], r10
vp_mul [r1 + 0x7f], r10
vp_mul [r1 + 0x7fff], r10

vp_mul [r15], r10
vp_mul [r15 + 0x7f], r10
vp_mul [r15 + 0x7fff], r10

vp_mul [r4], r10
vp_mul [r4 + 0x7f], r10
vp_mul [r4 + 0x7fff], r10

vp_mul [r12], r10
vp_mul [r12 + 0x7f], r10
vp_mul [r12 + 0x7fff], r10

def_func_end
