%include "inc/func.ninc"

def_func test

vp_shl 1, r0
vp_shl 1, r1
vp_shl 1, r2
vp_shl 1, r13
vp_shl 1, r14
vp_shl 1, r15

vp_shr 1, r0
vp_shr 1, r1
vp_shr 1, r2
vp_shr 1, r13
vp_shr 1, r14
vp_shr 1, r15

vp_asr 1, r0
vp_asr 1, r1
vp_asr 1, r2
vp_asr 1, r13
vp_asr 1, r14
vp_asr 1, r15

vp_shl 2, r0
vp_shl 3, r0
vp_shl 2, r15
vp_shl 3, r15

vp_shr 2, r0
vp_shr 3, r0
vp_shr 2, r15
vp_shr 3, r15

vp_asr 2, r0
vp_asr 3, r0
vp_asr 2, r15
vp_asr 3, r15

def_func_end
