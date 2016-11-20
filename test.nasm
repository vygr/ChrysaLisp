%include "inc/func.ninc"

def_func test

vp_shl r1, r0
vp_shl r1, r1
vp_shl r1, r2
vp_shl r1, r13
vp_shl r1, r14
vp_shl r1, r15

vp_shr r1, r0
vp_shr r1, r1
vp_shr r1, r2
vp_shr r1, r13
vp_shr r1, r14
vp_shr r1, r15

vp_asr r1, r0
vp_asr r1, r1
vp_asr r1, r2
vp_asr r1, r13
vp_asr r1, r14
vp_asr r1, r15

vp_shl r5, r0
vp_shl r5, r1
vp_shl r5, r2
vp_shl r5, r13
vp_shl r5, r14
vp_shl r5, r15

vp_shr r5, r0
vp_shr r5, r1
vp_shr r5, r2
vp_shr r5, r13
vp_shr r5, r14
vp_shr r5, r15

vp_asr r5, r0
vp_asr r5, r1
vp_asr r5, r2
vp_asr r5, r13
vp_asr r5, r14
vp_asr r5, r15

def_func_end
