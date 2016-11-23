%include "inc/func.ninc"

def_func test

;push and pop
vp_push r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15
vp_pop r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15

;const/reg
vp_cpy 0x7f, r0
vp_cpy 0x7f, r1
vp_cpy 0x7f, r2
vp_cpy 0x7f, r3
vp_cpy 0x7f, r4
vp_cpy 0x7f, r5
vp_cpy 0x7f, r6
vp_cpy 0x7f, r7
vp_cpy 0x7f, r8
vp_cpy 0x7f, r9
vp_cpy 0x7f, r10
vp_cpy 0x7f, r11
vp_cpy 0x7f, r12
vp_cpy 0x7f, r13
vp_cpy 0x7f, r14
vp_cpy 0x7f, r15

vp_cpy 0x80, r0
vp_cpy 0x80, r1
vp_cpy 0x80, r2
vp_cpy 0x80, r3
vp_cpy 0x80, r4
vp_cpy 0x80, r5
vp_cpy 0x80, r6
vp_cpy 0x80, r7
vp_cpy 0x80, r8
vp_cpy 0x80, r9
vp_cpy 0x80, r10
vp_cpy 0x80, r11
vp_cpy 0x80, r12
vp_cpy 0x80, r13
vp_cpy 0x80, r14
vp_cpy 0x80, r15

vp_cpy 0x8000ff, r0
vp_cpy 0x8000ff, r1
vp_cpy 0x8000ff, r2
vp_cpy 0x8000ff, r3
vp_cpy 0x8000ff, r4
vp_cpy 0x8000ff, r5
vp_cpy 0x8000ff, r6
vp_cpy 0x8000ff, r7
vp_cpy 0x8000ff, r8
vp_cpy 0x8000ff, r9
vp_cpy 0x8000ff, r10
vp_cpy 0x8000ff, r11
vp_cpy 0x8000ff, r12
vp_cpy 0x8000ff, r13
vp_cpy 0x8000ff, r14
vp_cpy 0x8000ff, r15

vp_cpy 0xfeff6ffff8fff9ff, r0
vp_cpy 0xfeff6ffff8fff9ff, r1
vp_cpy 0xfeff6ffff8fff9ff, r2
vp_cpy 0xfeff6ffff8fff9ff, r3
vp_cpy 0xfeff6ffff8fff9ff, r4
vp_cpy 0xfeff6ffff8fff9ff, r5
vp_cpy 0xfeff6ffff8fff9ff, r6
vp_cpy 0xfeff6ffff8fff9ff, r7
vp_cpy 0xfeff6ffff8fff9ff, r8
vp_cpy 0xfeff6ffff8fff9ff, r9
vp_cpy 0xfeff6ffff8fff9ff, r10
vp_cpy 0xfeff6ffff8fff9ff, r11
vp_cpy 0xfeff6ffff8fff9ff, r12
vp_cpy 0xfeff6ffff8fff9ff, r13
vp_cpy 0xfeff6ffff8fff9ff, r14
vp_cpy 0xfeff6ffff8fff9ff, r15

vp_add 0x7f, r0
vp_add 0x7f, r1
vp_add 0x7f, r2
vp_add 0x7f, r3
vp_add 0x7f, r4
vp_add 0x7f, r5
vp_add 0x7f, r6
vp_add 0x7f, r7
vp_add 0x7f, r8
vp_add 0x7f, r9
vp_add 0x7f, r10
vp_add 0x7f, r11
vp_add 0x7f, r12
vp_add 0x7f, r13
vp_add 0x7f, r14
vp_add 0x7f, r15

vp_add 0x7f4f5f6f, r0
vp_add 0x7f4f5f6f, r1
vp_add 0x7f4f5f6f, r2
vp_add 0x7f4f5f6f, r3
vp_add 0x7f4f5f6f, r4
vp_add 0x7f4f5f6f, r5
vp_add 0x7f4f5f6f, r6
vp_add 0x7f4f5f6f, r7
vp_add 0x7f4f5f6f, r8
vp_add 0x7f4f5f6f, r9
vp_add 0x7f4f5f6f, r10
vp_add 0x7f4f5f6f, r11
vp_add 0x7f4f5f6f, r12
vp_add 0x7f4f5f6f, r13
vp_add 0x7f4f5f6f, r14
vp_add 0x7f4f5f6f, r15

vp_sub 0x7f, r0
vp_sub 0x7f, r1
vp_sub 0x7f, r2
vp_sub 0x7f, r3
vp_sub 0x7f, r4
vp_sub 0x7f, r5
vp_sub 0x7f, r6
vp_sub 0x7f, r7
vp_sub 0x7f, r8
vp_sub 0x7f, r9
vp_sub 0x7f, r10
vp_sub 0x7f, r11
vp_sub 0x7f, r12
vp_sub 0x7f, r13
vp_sub 0x7f, r14
vp_sub 0x7f, r15

vp_sub 0x7f4f5f6f, r0
vp_sub 0x7f4f5f6f, r1
vp_sub 0x7f4f5f6f, r2
vp_sub 0x7f4f5f6f, r3
vp_sub 0x7f4f5f6f, r4
vp_sub 0x7f4f5f6f, r5
vp_sub 0x7f4f5f6f, r6
vp_sub 0x7f4f5f6f, r7
vp_sub 0x7f4f5f6f, r8
vp_sub 0x7f4f5f6f, r9
vp_sub 0x7f4f5f6f, r10
vp_sub 0x7f4f5f6f, r11
vp_sub 0x7f4f5f6f, r12
vp_sub 0x7f4f5f6f, r13
vp_sub 0x7f4f5f6f, r14
vp_sub 0x7f4f5f6f, r15

vp_cmp 0x7f, r0
vp_cmp 0x7f, r1
vp_cmp 0x7f, r2
vp_cmp 0x7f, r3
vp_cmp 0x7f, r4
vp_cmp 0x7f, r5
vp_cmp 0x7f, r6
vp_cmp 0x7f, r7
vp_cmp 0x7f, r8
vp_cmp 0x7f, r9
vp_cmp 0x7f, r10
vp_cmp 0x7f, r11
vp_cmp 0x7f, r12
vp_cmp 0x7f, r13
vp_cmp 0x7f, r14
vp_cmp 0x7f, r15

vp_cmp 0x7f4f5f6f, r0
vp_cmp 0x7f4f5f6f, r1
vp_cmp 0x7f4f5f6f, r2
vp_cmp 0x7f4f5f6f, r3
vp_cmp 0x7f4f5f6f, r4
vp_cmp 0x7f4f5f6f, r5
vp_cmp 0x7f4f5f6f, r6
vp_cmp 0x7f4f5f6f, r7
vp_cmp 0x7f4f5f6f, r8
vp_cmp 0x7f4f5f6f, r9
vp_cmp 0x7f4f5f6f, r10
vp_cmp 0x7f4f5f6f, r11
vp_cmp 0x7f4f5f6f, r12
vp_cmp 0x7f4f5f6f, r13
vp_cmp 0x7f4f5f6f, r14
vp_cmp 0x7f4f5f6f, r15

vp_and 0x7f, r0
vp_and 0x7f, r1
vp_and 0x7f, r2
vp_and 0x7f, r3
vp_and 0x7f, r4
vp_and 0x7f, r5
vp_and 0x7f, r6
vp_and 0x7f, r7
vp_and 0x7f, r8
vp_and 0x7f, r9
vp_and 0x7f, r10
vp_and 0x7f, r11
vp_and 0x7f, r12
vp_and 0x7f, r13
vp_and 0x7f, r14
vp_and 0x7f, r15

vp_and 0x7f4f5f6f, r0
vp_and 0x7f4f5f6f, r1
vp_and 0x7f4f5f6f, r2
vp_and 0x7f4f5f6f, r3
vp_and 0x7f4f5f6f, r4
vp_and 0x7f4f5f6f, r5
vp_and 0x7f4f5f6f, r6
vp_and 0x7f4f5f6f, r7
vp_and 0x7f4f5f6f, r8
vp_and 0x7f4f5f6f, r9
vp_and 0x7f4f5f6f, r10
vp_and 0x7f4f5f6f, r11
vp_and 0x7f4f5f6f, r12
vp_and 0x7f4f5f6f, r13
vp_and 0x7f4f5f6f, r14
vp_and 0x7f4f5f6f, r15

vp_or 0x7f, r0
vp_or 0x7f, r1
vp_or 0x7f, r2
vp_or 0x7f, r3
vp_or 0x7f, r4
vp_or 0x7f, r5
vp_or 0x7f, r6
vp_or 0x7f, r7
vp_or 0x7f, r8
vp_or 0x7f, r9
vp_or 0x7f, r10
vp_or 0x7f, r11
vp_or 0x7f, r12
vp_or 0x7f, r13
vp_or 0x7f, r14
vp_or 0x7f, r15

vp_or 0x7f4f5f6f, r0
vp_or 0x7f4f5f6f, r1
vp_or 0x7f4f5f6f, r2
vp_or 0x7f4f5f6f, r3
vp_or 0x7f4f5f6f, r4
vp_or 0x7f4f5f6f, r5
vp_or 0x7f4f5f6f, r6
vp_or 0x7f4f5f6f, r7
vp_or 0x7f4f5f6f, r8
vp_or 0x7f4f5f6f, r9
vp_or 0x7f4f5f6f, r10
vp_or 0x7f4f5f6f, r11
vp_or 0x7f4f5f6f, r12
vp_or 0x7f4f5f6f, r13
vp_or 0x7f4f5f6f, r14
vp_or 0x7f4f5f6f, r15

vp_xor 0x7f, r0
vp_xor 0x7f, r1
vp_xor 0x7f, r2
vp_xor 0x7f, r3
vp_xor 0x7f, r4
vp_xor 0x7f, r5
vp_xor 0x7f, r6
vp_xor 0x7f, r7
vp_xor 0x7f, r8
vp_xor 0x7f, r9
vp_xor 0x7f, r10
vp_xor 0x7f, r11
vp_xor 0x7f, r12
vp_xor 0x7f, r13
vp_xor 0x7f, r14
vp_xor 0x7f, r15

vp_xor 0x7f4f5f6f, r0
vp_xor 0x7f4f5f6f, r1
vp_xor 0x7f4f5f6f, r2
vp_xor 0x7f4f5f6f, r3
vp_xor 0x7f4f5f6f, r4
vp_xor 0x7f4f5f6f, r5
vp_xor 0x7f4f5f6f, r6
vp_xor 0x7f4f5f6f, r7
vp_xor 0x7f4f5f6f, r8
vp_xor 0x7f4f5f6f, r9
vp_xor 0x7f4f5f6f, r10
vp_xor 0x7f4f5f6f, r11
vp_xor 0x7f4f5f6f, r12
vp_xor 0x7f4f5f6f, r13
vp_xor 0x7f4f5f6f, r14
vp_xor 0x7f4f5f6f, r15

vp_mul 0x7f, r0
vp_mul 0x7f, r1
vp_mul 0x7f, r2
vp_mul 0x7f, r3
vp_mul 0x7f, r4
vp_mul 0x7f, r5
vp_mul 0x7f, r6
vp_mul 0x7f, r7
vp_mul 0x7f, r8
vp_mul 0x7f, r9
vp_mul 0x7f, r10
vp_mul 0x7f, r11
vp_mul 0x7f, r12
vp_mul 0x7f, r13
vp_mul 0x7f, r14
vp_mul 0x7f, r15

vp_mul 0x7f4f5f6f, r0
vp_mul 0x7f4f5f6f, r1
vp_mul 0x7f4f5f6f, r2
vp_mul 0x7f4f5f6f, r3
vp_mul 0x7f4f5f6f, r4
vp_mul 0x7f4f5f6f, r5
vp_mul 0x7f4f5f6f, r6
vp_mul 0x7f4f5f6f, r7
vp_mul 0x7f4f5f6f, r8
vp_mul 0x7f4f5f6f, r9
vp_mul 0x7f4f5f6f, r10
vp_mul 0x7f4f5f6f, r11
vp_mul 0x7f4f5f6f, r12
vp_mul 0x7f4f5f6f, r13
vp_mul 0x7f4f5f6f, r14
vp_mul 0x7f4f5f6f, r15

;inc/dec
vp_inc r0
vp_inc r1
vp_inc r2
vp_inc r3
vp_inc r4
vp_inc r5
vp_inc r6
vp_inc r7
vp_inc r8
vp_inc r9
vp_inc r10
vp_inc r11
vp_inc r12
vp_inc r13
vp_inc r14
vp_inc r15

vp_dec r0
vp_dec r1
vp_dec r2
vp_dec r3
vp_dec r4
vp_dec r5
vp_dec r6
vp_dec r7
vp_dec r8
vp_dec r9
vp_dec r10
vp_dec r11
vp_dec r12
vp_dec r13
vp_dec r14
vp_dec r15

;ret
vp_ret

;shifts
vp_shl 1, r0
vp_shl 1, r1
vp_shl 1, r2
vp_shl 1, r3
vp_shl 1, r4
vp_shl 1, r5
vp_shl 1, r6
vp_shl 1, r7
vp_shl 1, r8
vp_shl 1, r9
vp_shl 1, r10
vp_shl 1, r11
vp_shl 1, r12
vp_shl 1, r13
vp_shl 1, r14
vp_shl 1, r15

vp_shl 2, r0
vp_shl 3, r1
vp_shl 4, r2
vp_shl 5, r3
vp_shl 6, r4
vp_shl 7, r5
vp_shl 8, r6
vp_shl 9, r7
vp_shl 10, r8
vp_shl 11, r9
vp_shl 12, r10
vp_shl 13, r11
vp_shl 14, r12
vp_shl 15, r13
vp_shl 16, r14
vp_shl 17, r15

vp_shl r15, r0
vp_shl r14, r1
vp_shl r13, r2
vp_shl r12, r3
vp_shl r11, r4
vp_shl r10, r5
vp_shl r9, r6
vp_shl r8, r7
vp_shl r7, r8
vp_shl r6, r9
vp_shl r5, r10
vp_shl r4, r11
vp_shl r3, r12
vp_shl r2, r13
vp_shl r1, r14
vp_shl r0, r15

vp_shr 1, r0
vp_shr 1, r1
vp_shr 1, r2
vp_shr 1, r3
vp_shr 1, r4
vp_shr 1, r5
vp_shr 1, r6
vp_shr 1, r7
vp_shr 1, r8
vp_shr 1, r9
vp_shr 1, r10
vp_shr 1, r11
vp_shr 1, r12
vp_shr 1, r13
vp_shr 1, r14
vp_shr 1, r15

vp_shr 2, r0
vp_shr 3, r1
vp_shr 4, r2
vp_shr 5, r3
vp_shr 6, r4
vp_shr 7, r5
vp_shr 8, r6
vp_shr 9, r7
vp_shr 10, r8
vp_shr 11, r9
vp_shr 12, r10
vp_shr 13, r11
vp_shr 14, r12
vp_shr 15, r13
vp_shr 16, r14
vp_shr 17, r15

vp_shr r15, r0
vp_shr r14, r1
vp_shr r13, r2
vp_shr r12, r3
vp_shr r11, r4
vp_shr r10, r5
vp_shr r9, r6
vp_shr r8, r7
vp_shr r7, r8
vp_shr r6, r9
vp_shr r5, r10
vp_shr r4, r11
vp_shr r3, r12
vp_shr r2, r13
vp_shr r1, r14
vp_shr r0, r15

vp_asr 1, r0
vp_asr 1, r1
vp_asr 1, r2
vp_asr 1, r3
vp_asr 1, r4
vp_asr 1, r5
vp_asr 1, r6
vp_asr 1, r7
vp_asr 1, r8
vp_asr 1, r9
vp_asr 1, r10
vp_asr 1, r11
vp_asr 1, r12
vp_asr 1, r13
vp_asr 1, r14
vp_asr 1, r15

vp_asr 2, r0
vp_asr 3, r1
vp_asr 4, r2
vp_asr 5, r3
vp_asr 6, r4
vp_asr 7, r5
vp_asr 8, r6
vp_asr 9, r7
vp_asr 10, r8
vp_asr 11, r9
vp_asr 12, r10
vp_asr 13, r11
vp_asr 14, r12
vp_asr 15, r13
vp_asr 16, r14
vp_asr 17, r15

vp_asr r15, r0
vp_asr r14, r1
vp_asr r13, r2
vp_asr r12, r3
vp_asr r11, r4
vp_asr r10, r5
vp_asr r9, r6
vp_asr r8, r7
vp_asr r7, r8
vp_asr r6, r9
vp_asr r5, r10
vp_asr r4, r11
vp_asr r3, r12
vp_asr r2, r13
vp_asr r1, r14
vp_asr r0, r15

;ir
vp_cpy [r5], r0
vp_cpy [r6], r1
vp_cpy [r7], r2
vp_cpy [r8], r3
vp_cpy [r9], r4
vp_cpy [r10], r5
vp_cpy [r11], r6
vp_cpy [r12], r7
vp_cpy [r13], r8
vp_cpy [r14], r9
vp_cpy [r15], r10
vp_cpy [r0], r11
vp_cpy [r1], r12
vp_cpy [r2], r13
vp_cpy [r3], r14
vp_cpy [r4], r15

vp_cpy [r5 + 0x7f], r0
vp_cpy [r6 + 0x7f], r1
vp_cpy [r7 + 0x7f], r2
vp_cpy [r8 + 0x7f], r3
vp_cpy [r9 + 0x7f], r4
vp_cpy [r10 + 0x7f], r5
vp_cpy [r11 + 0x7f], r6
vp_cpy [r12 + 0x7f], r7
vp_cpy [r13 + 0x7f], r8
vp_cpy [r14 + 0x7f], r9
vp_cpy [r15 + 0x7f], r10
vp_cpy [r0 + 0x7f], r11
vp_cpy [r1 + 0x7f], r12
vp_cpy [r2 + 0x7f], r13
vp_cpy [r3 + 0x7f], r14
vp_cpy [r4 + 0x7f], r15

vp_cpy [r5 + 0x80], r0
vp_cpy [r6 + 0x80], r1
vp_cpy [r7 + 0x80], r2
vp_cpy [r8 + 0x80], r3
vp_cpy [r9 + 0x80], r4
vp_cpy [r10 + 0x80], r5
vp_cpy [r11 + 0x80], r6
vp_cpy [r12 + 0x80], r7
vp_cpy [r13 + 0x80], r8
vp_cpy [r14 + 0x80], r9
vp_cpy [r15 + 0x80], r10
vp_cpy [r0 + 0x80], r11
vp_cpy [r1 + 0x80], r12
vp_cpy [r2 + 0x80], r13
vp_cpy [r3 + 0x80], r14
vp_cpy [r4 + 0x80], r15

vp_cpy [r5 + 0x7fab1256], r0
vp_cpy [r6 + 0x7fab1256], r1
vp_cpy [r7 + 0x7fab1256], r2
vp_cpy [r8 + 0x7fab1256], r3
vp_cpy [r9 + 0x7fab1256], r4
vp_cpy [r10 + 0x7fab1256], r5
vp_cpy [r11 + 0x7fab1256], r6
vp_cpy [r12 + 0x7fab1256], r7
vp_cpy [r13 + 0x7fab1256], r8
vp_cpy [r14 + 0x7fab1256], r9
vp_cpy [r15 + 0x7fab1256], r10
vp_cpy [r0 + 0x7fab1256], r11
vp_cpy [r1 + 0x7fab1256], r12
vp_cpy [r2 + 0x7fab1256], r13
vp_cpy [r3 + 0x7fab1256], r14
vp_cpy [r4 + 0x7fab1256], r15

vp_add [r5], r0
vp_add [r6], r1
vp_add [r7], r2
vp_add [r8], r3
vp_add [r9], r4
vp_add [r10], r5
vp_add [r11], r6
vp_add [r12], r7
vp_add [r13], r8
vp_add [r14], r9
vp_add [r15], r10
vp_add [r0], r11
vp_add [r1], r12
vp_add [r2], r13
vp_add [r3], r14
vp_add [r4], r15

vp_add [r5 + 0x7f], r0
vp_add [r6 + 0x7f], r1
vp_add [r7 + 0x7f], r2
vp_add [r8 + 0x7f], r3
vp_add [r9 + 0x7f], r4
vp_add [r10 + 0x7f], r5
vp_add [r11 + 0x7f], r6
vp_add [r12 + 0x7f], r7
vp_add [r13 + 0x7f], r8
vp_add [r14 + 0x7f], r9
vp_add [r15 + 0x7f], r10
vp_add [r0 + 0x7f], r11
vp_add [r1 + 0x7f], r12
vp_add [r2 + 0x7f], r13
vp_add [r3 + 0x7f], r14
vp_add [r4 + 0x7f], r15

vp_add [r5 + 0x80], r0
vp_add [r6 + 0x80], r1
vp_add [r7 + 0x80], r2
vp_add [r8 + 0x80], r3
vp_add [r9 + 0x80], r4
vp_add [r10 + 0x80], r5
vp_add [r11 + 0x80], r6
vp_add [r12 + 0x80], r7
vp_add [r13 + 0x80], r8
vp_add [r14 + 0x80], r9
vp_add [r15 + 0x80], r10
vp_add [r0 + 0x80], r11
vp_add [r1 + 0x80], r12
vp_add [r2 + 0x80], r13
vp_add [r3 + 0x80], r14
vp_add [r4 + 0x80], r15

vp_add [r5 + 0x7fab1256], r0
vp_add [r6 + 0x7fab1256], r1
vp_add [r7 + 0x7fab1256], r2
vp_add [r8 + 0x7fab1256], r3
vp_add [r9 + 0x7fab1256], r4
vp_add [r10 + 0x7fab1256], r5
vp_add [r11 + 0x7fab1256], r6
vp_add [r12 + 0x7fab1256], r7
vp_add [r13 + 0x7fab1256], r8
vp_add [r14 + 0x7fab1256], r9
vp_add [r15 + 0x7fab1256], r10
vp_add [r0 + 0x7fab1256], r11
vp_add [r1 + 0x7fab1256], r12
vp_add [r2 + 0x7fab1256], r13
vp_add [r3 + 0x7fab1256], r14
vp_add [r4 + 0x7fab1256], r15

vp_sub [r5], r0
vp_sub [r6], r1
vp_sub [r7], r2
vp_sub [r8], r3
vp_sub [r9], r4
vp_sub [r10], r5
vp_sub [r11], r6
vp_sub [r12], r7
vp_sub [r13], r8
vp_sub [r14], r9
vp_sub [r15], r10
vp_sub [r0], r11
vp_sub [r1], r12
vp_sub [r2], r13
vp_sub [r3], r14
vp_sub [r4], r15

vp_sub [r5 + 0x7f], r0
vp_sub [r6 + 0x7f], r1
vp_sub [r7 + 0x7f], r2
vp_sub [r8 + 0x7f], r3
vp_sub [r9 + 0x7f], r4
vp_sub [r10 + 0x7f], r5
vp_sub [r11 + 0x7f], r6
vp_sub [r12 + 0x7f], r7
vp_sub [r13 + 0x7f], r8
vp_sub [r14 + 0x7f], r9
vp_sub [r15 + 0x7f], r10
vp_sub [r0 + 0x7f], r11
vp_sub [r1 + 0x7f], r12
vp_sub [r2 + 0x7f], r13
vp_sub [r3 + 0x7f], r14
vp_sub [r4 + 0x7f], r15

vp_sub [r5 + 0x80], r0
vp_sub [r6 + 0x80], r1
vp_sub [r7 + 0x80], r2
vp_sub [r8 + 0x80], r3
vp_sub [r9 + 0x80], r4
vp_sub [r10 + 0x80], r5
vp_sub [r11 + 0x80], r6
vp_sub [r12 + 0x80], r7
vp_sub [r13 + 0x80], r8
vp_sub [r14 + 0x80], r9
vp_sub [r15 + 0x80], r10
vp_sub [r0 + 0x80], r11
vp_sub [r1 + 0x80], r12
vp_sub [r2 + 0x80], r13
vp_sub [r3 + 0x80], r14
vp_sub [r4 + 0x80], r15

vp_sub [r5 + 0x7fab1256], r0
vp_sub [r6 + 0x7fab1256], r1
vp_sub [r7 + 0x7fab1256], r2
vp_sub [r8 + 0x7fab1256], r3
vp_sub [r9 + 0x7fab1256], r4
vp_sub [r10 + 0x7fab1256], r5
vp_sub [r11 + 0x7fab1256], r6
vp_sub [r12 + 0x7fab1256], r7
vp_sub [r13 + 0x7fab1256], r8
vp_sub [r14 + 0x7fab1256], r9
vp_sub [r15 + 0x7fab1256], r10
vp_sub [r0 + 0x7fab1256], r11
vp_sub [r1 + 0x7fab1256], r12
vp_sub [r2 + 0x7fab1256], r13
vp_sub [r3 + 0x7fab1256], r14
vp_sub [r4 + 0x7fab1256], r15

vp_cmp [r5], r0
vp_cmp [r6], r1
vp_cmp [r7], r2
vp_cmp [r8], r3
vp_cmp [r9], r4
vp_cmp [r10], r5
vp_cmp [r11], r6
vp_cmp [r12], r7
vp_cmp [r13], r8
vp_cmp [r14], r9
vp_cmp [r15], r10
vp_cmp [r0], r11
vp_cmp [r1], r12
vp_cmp [r2], r13
vp_cmp [r3], r14
vp_cmp [r4], r15

vp_cmp [r5 + 0x7f], r0
vp_cmp [r6 + 0x7f], r1
vp_cmp [r7 + 0x7f], r2
vp_cmp [r8 + 0x7f], r3
vp_cmp [r9 + 0x7f], r4
vp_cmp [r10 + 0x7f], r5
vp_cmp [r11 + 0x7f], r6
vp_cmp [r12 + 0x7f], r7
vp_cmp [r13 + 0x7f], r8
vp_cmp [r14 + 0x7f], r9
vp_cmp [r15 + 0x7f], r10
vp_cmp [r0 + 0x7f], r11
vp_cmp [r1 + 0x7f], r12
vp_cmp [r2 + 0x7f], r13
vp_cmp [r3 + 0x7f], r14
vp_cmp [r4 + 0x7f], r15

vp_cmp [r5 + 0x80], r0
vp_cmp [r6 + 0x80], r1
vp_cmp [r7 + 0x80], r2
vp_cmp [r8 + 0x80], r3
vp_cmp [r9 + 0x80], r4
vp_cmp [r10 + 0x80], r5
vp_cmp [r11 + 0x80], r6
vp_cmp [r12 + 0x80], r7
vp_cmp [r13 + 0x80], r8
vp_cmp [r14 + 0x80], r9
vp_cmp [r15 + 0x80], r10
vp_cmp [r0 + 0x80], r11
vp_cmp [r1 + 0x80], r12
vp_cmp [r2 + 0x80], r13
vp_cmp [r3 + 0x80], r14
vp_cmp [r4 + 0x80], r15

vp_cmp [r5 + 0x7fab1256], r0
vp_cmp [r6 + 0x7fab1256], r1
vp_cmp [r7 + 0x7fab1256], r2
vp_cmp [r8 + 0x7fab1256], r3
vp_cmp [r9 + 0x7fab1256], r4
vp_cmp [r10 + 0x7fab1256], r5
vp_cmp [r11 + 0x7fab1256], r6
vp_cmp [r12 + 0x7fab1256], r7
vp_cmp [r13 + 0x7fab1256], r8
vp_cmp [r14 + 0x7fab1256], r9
vp_cmp [r15 + 0x7fab1256], r10
vp_cmp [r0 + 0x7fab1256], r11
vp_cmp [r1 + 0x7fab1256], r12
vp_cmp [r2 + 0x7fab1256], r13
vp_cmp [r3 + 0x7fab1256], r14
vp_cmp [r4 + 0x7fab1256], r15

vp_and [r5], r0
vp_and [r6], r1
vp_and [r7], r2
vp_and [r8], r3
vp_and [r9], r4
vp_and [r10], r5
vp_and [r11], r6
vp_and [r12], r7
vp_and [r13], r8
vp_and [r14], r9
vp_and [r15], r10
vp_and [r0], r11
vp_and [r1], r12
vp_and [r2], r13
vp_and [r3], r14
vp_and [r4], r15

vp_and [r5 + 0x7f], r0
vp_and [r6 + 0x7f], r1
vp_and [r7 + 0x7f], r2
vp_and [r8 + 0x7f], r3
vp_and [r9 + 0x7f], r4
vp_and [r10 + 0x7f], r5
vp_and [r11 + 0x7f], r6
vp_and [r12 + 0x7f], r7
vp_and [r13 + 0x7f], r8
vp_and [r14 + 0x7f], r9
vp_and [r15 + 0x7f], r10
vp_and [r0 + 0x7f], r11
vp_and [r1 + 0x7f], r12
vp_and [r2 + 0x7f], r13
vp_and [r3 + 0x7f], r14
vp_and [r4 + 0x7f], r15

vp_and [r5 + 0x80], r0
vp_and [r6 + 0x80], r1
vp_and [r7 + 0x80], r2
vp_and [r8 + 0x80], r3
vp_and [r9 + 0x80], r4
vp_and [r10 + 0x80], r5
vp_and [r11 + 0x80], r6
vp_and [r12 + 0x80], r7
vp_and [r13 + 0x80], r8
vp_and [r14 + 0x80], r9
vp_and [r15 + 0x80], r10
vp_and [r0 + 0x80], r11
vp_and [r1 + 0x80], r12
vp_and [r2 + 0x80], r13
vp_and [r3 + 0x80], r14
vp_and [r4 + 0x80], r15

vp_and [r5 + 0x7fab1256], r0
vp_and [r6 + 0x7fab1256], r1
vp_and [r7 + 0x7fab1256], r2
vp_and [r8 + 0x7fab1256], r3
vp_and [r9 + 0x7fab1256], r4
vp_and [r10 + 0x7fab1256], r5
vp_and [r11 + 0x7fab1256], r6
vp_and [r12 + 0x7fab1256], r7
vp_and [r13 + 0x7fab1256], r8
vp_and [r14 + 0x7fab1256], r9
vp_and [r15 + 0x7fab1256], r10
vp_and [r0 + 0x7fab1256], r11
vp_and [r1 + 0x7fab1256], r12
vp_and [r2 + 0x7fab1256], r13
vp_and [r3 + 0x7fab1256], r14
vp_and [r4 + 0x7fab1256], r15

vp_or [r5], r0
vp_or [r6], r1
vp_or [r7], r2
vp_or [r8], r3
vp_or [r9], r4
vp_or [r10], r5
vp_or [r11], r6
vp_or [r12], r7
vp_or [r13], r8
vp_or [r14], r9
vp_or [r15], r10
vp_or [r0], r11
vp_or [r1], r12
vp_or [r2], r13
vp_or [r3], r14
vp_or [r4], r15

vp_or [r5 + 0x7f], r0
vp_or [r6 + 0x7f], r1
vp_or [r7 + 0x7f], r2
vp_or [r8 + 0x7f], r3
vp_or [r9 + 0x7f], r4
vp_or [r10 + 0x7f], r5
vp_or [r11 + 0x7f], r6
vp_or [r12 + 0x7f], r7
vp_or [r13 + 0x7f], r8
vp_or [r14 + 0x7f], r9
vp_or [r15 + 0x7f], r10
vp_or [r0 + 0x7f], r11
vp_or [r1 + 0x7f], r12
vp_or [r2 + 0x7f], r13
vp_or [r3 + 0x7f], r14
vp_or [r4 + 0x7f], r15

vp_or [r5 + 0x80], r0
vp_or [r6 + 0x80], r1
vp_or [r7 + 0x80], r2
vp_or [r8 + 0x80], r3
vp_or [r9 + 0x80], r4
vp_or [r10 + 0x80], r5
vp_or [r11 + 0x80], r6
vp_or [r12 + 0x80], r7
vp_or [r13 + 0x80], r8
vp_or [r14 + 0x80], r9
vp_or [r15 + 0x80], r10
vp_or [r0 + 0x80], r11
vp_or [r1 + 0x80], r12
vp_or [r2 + 0x80], r13
vp_or [r3 + 0x80], r14
vp_or [r4 + 0x80], r15

vp_or [r5 + 0x7fab1256], r0
vp_or [r6 + 0x7fab1256], r1
vp_or [r7 + 0x7fab1256], r2
vp_or [r8 + 0x7fab1256], r3
vp_or [r9 + 0x7fab1256], r4
vp_or [r10 + 0x7fab1256], r5
vp_or [r11 + 0x7fab1256], r6
vp_or [r12 + 0x7fab1256], r7
vp_or [r13 + 0x7fab1256], r8
vp_or [r14 + 0x7fab1256], r9
vp_or [r15 + 0x7fab1256], r10
vp_or [r0 + 0x7fab1256], r11
vp_or [r1 + 0x7fab1256], r12
vp_or [r2 + 0x7fab1256], r13
vp_or [r3 + 0x7fab1256], r14
vp_or [r4 + 0x7fab1256], r15

vp_xor [r5], r0
vp_xor [r6], r1
vp_xor [r7], r2
vp_xor [r8], r3
vp_xor [r9], r4
vp_xor [r10], r5
vp_xor [r11], r6
vp_xor [r12], r7
vp_xor [r13], r8
vp_xor [r14], r9
vp_xor [r15], r10
vp_xor [r0], r11
vp_xor [r1], r12
vp_xor [r2], r13
vp_xor [r3], r14
vp_xor [r4], r15

vp_xor [r5 + 0x7f], r0
vp_xor [r6 + 0x7f], r1
vp_xor [r7 + 0x7f], r2
vp_xor [r8 + 0x7f], r3
vp_xor [r9 + 0x7f], r4
vp_xor [r10 + 0x7f], r5
vp_xor [r11 + 0x7f], r6
vp_xor [r12 + 0x7f], r7
vp_xor [r13 + 0x7f], r8
vp_xor [r14 + 0x7f], r9
vp_xor [r15 + 0x7f], r10
vp_xor [r0 + 0x7f], r11
vp_xor [r1 + 0x7f], r12
vp_xor [r2 + 0x7f], r13
vp_xor [r3 + 0x7f], r14
vp_xor [r4 + 0x7f], r15

vp_xor [r5 + 0x80], r0
vp_xor [r6 + 0x80], r1
vp_xor [r7 + 0x80], r2
vp_xor [r8 + 0x80], r3
vp_xor [r9 + 0x80], r4
vp_xor [r10 + 0x80], r5
vp_xor [r11 + 0x80], r6
vp_xor [r12 + 0x80], r7
vp_xor [r13 + 0x80], r8
vp_xor [r14 + 0x80], r9
vp_xor [r15 + 0x80], r10
vp_xor [r0 + 0x80], r11
vp_xor [r1 + 0x80], r12
vp_xor [r2 + 0x80], r13
vp_xor [r3 + 0x80], r14
vp_xor [r4 + 0x80], r15

vp_xor [r5 + 0x7fab1256], r0
vp_xor [r6 + 0x7fab1256], r1
vp_xor [r7 + 0x7fab1256], r2
vp_xor [r8 + 0x7fab1256], r3
vp_xor [r9 + 0x7fab1256], r4
vp_xor [r10 + 0x7fab1256], r5
vp_xor [r11 + 0x7fab1256], r6
vp_xor [r12 + 0x7fab1256], r7
vp_xor [r13 + 0x7fab1256], r8
vp_xor [r14 + 0x7fab1256], r9
vp_xor [r15 + 0x7fab1256], r10
vp_xor [r0 + 0x7fab1256], r11
vp_xor [r1 + 0x7fab1256], r12
vp_xor [r2 + 0x7fab1256], r13
vp_xor [r3 + 0x7fab1256], r14
vp_xor [r4 + 0x7fab1256], r15

vp_mul [r5], r0
vp_mul [r6], r1
vp_mul [r7], r2
vp_mul [r8], r3
vp_mul [r9], r4
vp_mul [r10], r5
vp_mul [r11], r6
vp_mul [r12], r7
vp_mul [r13], r8
vp_mul [r14], r9
vp_mul [r15], r10
vp_mul [r0], r11
vp_mul [r1], r12
vp_mul [r2], r13
vp_mul [r3], r14
vp_mul [r4], r15

vp_mul [r5 + 0x7f], r0
vp_mul [r6 + 0x7f], r1
vp_mul [r7 + 0x7f], r2
vp_mul [r8 + 0x7f], r3
vp_mul [r9 + 0x7f], r4
vp_mul [r10 + 0x7f], r5
vp_mul [r11 + 0x7f], r6
vp_mul [r12 + 0x7f], r7
vp_mul [r13 + 0x7f], r8
vp_mul [r14 + 0x7f], r9
vp_mul [r15 + 0x7f], r10
vp_mul [r0 + 0x7f], r11
vp_mul [r1 + 0x7f], r12
vp_mul [r2 + 0x7f], r13
vp_mul [r3 + 0x7f], r14
vp_mul [r4 + 0x7f], r15

vp_mul [r5 + 0x80], r0
vp_mul [r6 + 0x80], r1
vp_mul [r7 + 0x80], r2
vp_mul [r8 + 0x80], r3
vp_mul [r9 + 0x80], r4
vp_mul [r10 + 0x80], r5
vp_mul [r11 + 0x80], r6
vp_mul [r12 + 0x80], r7
vp_mul [r13 + 0x80], r8
vp_mul [r14 + 0x80], r9
vp_mul [r15 + 0x80], r10
vp_mul [r0 + 0x80], r11
vp_mul [r1 + 0x80], r12
vp_mul [r2 + 0x80], r13
vp_mul [r3 + 0x80], r14
vp_mul [r4 + 0x80], r15

vp_mul [r5 + 0x7fab1256], r0
vp_mul [r6 + 0x7fab1256], r1
vp_mul [r7 + 0x7fab1256], r2
vp_mul [r8 + 0x7fab1256], r3
vp_mul [r9 + 0x7fab1256], r4
vp_mul [r10 + 0x7fab1256], r5
vp_mul [r11 + 0x7fab1256], r6
vp_mul [r12 + 0x7fab1256], r7
vp_mul [r13 + 0x7fab1256], r8
vp_mul [r14 + 0x7fab1256], r9
vp_mul [r15 + 0x7fab1256], r10
vp_mul [r0 + 0x7fab1256], r11
vp_mul [r1 + 0x7fab1256], r12
vp_mul [r2 + 0x7fab1256], r13
vp_mul [r3 + 0x7fab1256], r14
vp_mul [r4 + 0x7fab1256], r15

;irb
vp_cpy_b [r5], r0
vp_cpy_b [r6], r1
vp_cpy_b [r7], r2
vp_cpy_b [r8], r3
vp_cpy_b [r9], r4
vp_cpy_b [r10], r5
vp_cpy_b [r11], r6
vp_cpy_b [r12], r7
vp_cpy_b [r13], r8
vp_cpy_b [r14], r9
vp_cpy_b [r15], r10
vp_cpy_b [r0], r11
vp_cpy_b [r1], r12
vp_cpy_b [r2], r13
vp_cpy_b [r3], r14
vp_cpy_b [r4], r15

vp_cpy_b [r5 + 0x7f], r0
vp_cpy_b [r6 + 0x7f], r1
vp_cpy_b [r7 + 0x7f], r2
vp_cpy_b [r8 + 0x7f], r3
vp_cpy_b [r9 + 0x7f], r4
vp_cpy_b [r10 + 0x7f], r5
vp_cpy_b [r11 + 0x7f], r6
vp_cpy_b [r12 + 0x7f], r7
vp_cpy_b [r13 + 0x7f], r8
vp_cpy_b [r14 + 0x7f], r9
vp_cpy_b [r15 + 0x7f], r10
vp_cpy_b [r0 + 0x7f], r11
vp_cpy_b [r1 + 0x7f], r12
vp_cpy_b [r2 + 0x7f], r13
vp_cpy_b [r3 + 0x7f], r14
vp_cpy_b [r4 + 0x7f], r15

vp_cpy_b [r5 + 0x80], r0
vp_cpy_b [r6 + 0x80], r1
vp_cpy_b [r7 + 0x80], r2
vp_cpy_b [r8 + 0x80], r3
vp_cpy_b [r9 + 0x80], r4
vp_cpy_b [r10 + 0x80], r5
vp_cpy_b [r11 + 0x80], r6
vp_cpy_b [r12 + 0x80], r7
vp_cpy_b [r13 + 0x80], r8
vp_cpy_b [r14 + 0x80], r9
vp_cpy_b [r15 + 0x80], r10
vp_cpy_b [r0 + 0x80], r11
vp_cpy_b [r1 + 0x80], r12
vp_cpy_b [r2 + 0x80], r13
vp_cpy_b [r3 + 0x80], r14
vp_cpy_b [r4 + 0x80], r15

vp_cpy_b [r5 + 0x7fab1256], r0
vp_cpy_b [r6 + 0x7fab1256], r1
vp_cpy_b [r7 + 0x7fab1256], r2
vp_cpy_b [r8 + 0x7fab1256], r3
vp_cpy_b [r9 + 0x7fab1256], r4
vp_cpy_b [r10 + 0x7fab1256], r5
vp_cpy_b [r11 + 0x7fab1256], r6
vp_cpy_b [r12 + 0x7fab1256], r7
vp_cpy_b [r13 + 0x7fab1256], r8
vp_cpy_b [r14 + 0x7fab1256], r9
vp_cpy_b [r15 + 0x7fab1256], r10
vp_cpy_b [r0 + 0x7fab1256], r11
vp_cpy_b [r1 + 0x7fab1256], r12
vp_cpy_b [r2 + 0x7fab1256], r13
vp_cpy_b [r3 + 0x7fab1256], r14
vp_cpy_b [r4 + 0x7fab1256], r15

;irs
vp_cpy_s [r5], r0
vp_cpy_s [r6], r1
vp_cpy_s [r7], r2
vp_cpy_s [r8], r3
vp_cpy_s [r9], r4
vp_cpy_s [r10], r5
vp_cpy_s [r11], r6
vp_cpy_s [r12], r7
vp_cpy_s [r13], r8
vp_cpy_s [r14], r9
vp_cpy_s [r15], r10
vp_cpy_s [r0], r11
vp_cpy_s [r1], r12
vp_cpy_s [r2], r13
vp_cpy_s [r3], r14
vp_cpy_s [r4], r15

vp_cpy_s [r5 + 0x7f], r0
vp_cpy_s [r6 + 0x7f], r1
vp_cpy_s [r7 + 0x7f], r2
vp_cpy_s [r8 + 0x7f], r3
vp_cpy_s [r9 + 0x7f], r4
vp_cpy_s [r10 + 0x7f], r5
vp_cpy_s [r11 + 0x7f], r6
vp_cpy_s [r12 + 0x7f], r7
vp_cpy_s [r13 + 0x7f], r8
vp_cpy_s [r14 + 0x7f], r9
vp_cpy_s [r15 + 0x7f], r10
vp_cpy_s [r0 + 0x7f], r11
vp_cpy_s [r1 + 0x7f], r12
vp_cpy_s [r2 + 0x7f], r13
vp_cpy_s [r3 + 0x7f], r14
vp_cpy_s [r4 + 0x7f], r15

vp_cpy_s [r5 + 0x80], r0
vp_cpy_s [r6 + 0x80], r1
vp_cpy_s [r7 + 0x80], r2
vp_cpy_s [r8 + 0x80], r3
vp_cpy_s [r9 + 0x80], r4
vp_cpy_s [r10 + 0x80], r5
vp_cpy_s [r11 + 0x80], r6
vp_cpy_s [r12 + 0x80], r7
vp_cpy_s [r13 + 0x80], r8
vp_cpy_s [r14 + 0x80], r9
vp_cpy_s [r15 + 0x80], r10
vp_cpy_s [r0 + 0x80], r11
vp_cpy_s [r1 + 0x80], r12
vp_cpy_s [r2 + 0x80], r13
vp_cpy_s [r3 + 0x80], r14
vp_cpy_s [r4 + 0x80], r15

vp_cpy_s [r5 + 0x7fab1256], r0
vp_cpy_s [r6 + 0x7fab1256], r1
vp_cpy_s [r7 + 0x7fab1256], r2
vp_cpy_s [r8 + 0x7fab1256], r3
vp_cpy_s [r9 + 0x7fab1256], r4
vp_cpy_s [r10 + 0x7fab1256], r5
vp_cpy_s [r11 + 0x7fab1256], r6
vp_cpy_s [r12 + 0x7fab1256], r7
vp_cpy_s [r13 + 0x7fab1256], r8
vp_cpy_s [r14 + 0x7fab1256], r9
vp_cpy_s [r15 + 0x7fab1256], r10
vp_cpy_s [r0 + 0x7fab1256], r11
vp_cpy_s [r1 + 0x7fab1256], r12
vp_cpy_s [r2 + 0x7fab1256], r13
vp_cpy_s [r3 + 0x7fab1256], r14
vp_cpy_s [r4 + 0x7fab1256], r15

;iri
vp_cpy_i [r5], r0
vp_cpy_i [r6], r1
vp_cpy_i [r7], r2
vp_cpy_i [r8], r3
vp_cpy_i [r9], r4
vp_cpy_i [r10], r5
vp_cpy_i [r11], r6
vp_cpy_i [r12], r7
vp_cpy_i [r13], r8
vp_cpy_i [r14], r9
vp_cpy_i [r15], r10
vp_cpy_i [r0], r11
vp_cpy_i [r1], r12
vp_cpy_i [r2], r13
vp_cpy_i [r3], r14
vp_cpy_i [r4], r15

vp_cpy_i [r5 + 0x7f], r0
vp_cpy_i [r6 + 0x7f], r1
vp_cpy_i [r7 + 0x7f], r2
vp_cpy_i [r8 + 0x7f], r3
vp_cpy_i [r9 + 0x7f], r4
vp_cpy_i [r10 + 0x7f], r5
vp_cpy_i [r11 + 0x7f], r6
vp_cpy_i [r12 + 0x7f], r7
vp_cpy_i [r13 + 0x7f], r8
vp_cpy_i [r14 + 0x7f], r9
vp_cpy_i [r15 + 0x7f], r10
vp_cpy_i [r0 + 0x7f], r11
vp_cpy_i [r1 + 0x7f], r12
vp_cpy_i [r2 + 0x7f], r13
vp_cpy_i [r3 + 0x7f], r14
vp_cpy_i [r4 + 0x7f], r15

vp_cpy_i [r5 + 0x80], r0
vp_cpy_i [r6 + 0x80], r1
vp_cpy_i [r7 + 0x80], r2
vp_cpy_i [r8 + 0x80], r3
vp_cpy_i [r9 + 0x80], r4
vp_cpy_i [r10 + 0x80], r5
vp_cpy_i [r11 + 0x80], r6
vp_cpy_i [r12 + 0x80], r7
vp_cpy_i [r13 + 0x80], r8
vp_cpy_i [r14 + 0x80], r9
vp_cpy_i [r15 + 0x80], r10
vp_cpy_i [r0 + 0x80], r11
vp_cpy_i [r1 + 0x80], r12
vp_cpy_i [r2 + 0x80], r13
vp_cpy_i [r3 + 0x80], r14
vp_cpy_i [r4 + 0x80], r15

vp_cpy_i [r5 + 0x7fab1256], r0
vp_cpy_i [r6 + 0x7fab1256], r1
vp_cpy_i [r7 + 0x7fab1256], r2
vp_cpy_i [r8 + 0x7fab1256], r3
vp_cpy_i [r9 + 0x7fab1256], r4
vp_cpy_i [r10 + 0x7fab1256], r5
vp_cpy_i [r11 + 0x7fab1256], r6
vp_cpy_i [r12 + 0x7fab1256], r7
vp_cpy_i [r13 + 0x7fab1256], r8
vp_cpy_i [r14 + 0x7fab1256], r9
vp_cpy_i [r15 + 0x7fab1256], r10
vp_cpy_i [r0 + 0x7fab1256], r11
vp_cpy_i [r1 + 0x7fab1256], r12
vp_cpy_i [r2 + 0x7fab1256], r13
vp_cpy_i [r3 + 0x7fab1256], r14
vp_cpy_i [r4 + 0x7fab1256], r15

;irub
vp_cpy_ub [r5], r0
vp_cpy_ub [r6], r1
vp_cpy_ub [r7], r2
vp_cpy_ub [r8], r3
vp_cpy_ub [r9], r4
vp_cpy_ub [r10], r5
vp_cpy_ub [r11], r6
vp_cpy_ub [r12], r7
vp_cpy_ub [r13], r8
vp_cpy_ub [r14], r9
vp_cpy_ub [r15], r10
vp_cpy_ub [r0], r11
vp_cpy_ub [r1], r12
vp_cpy_ub [r2], r13
vp_cpy_ub [r3], r14
vp_cpy_ub [r4], r15

vp_cpy_ub [r5 + 0x7f], r0
vp_cpy_ub [r6 + 0x7f], r1
vp_cpy_ub [r7 + 0x7f], r2
vp_cpy_ub [r8 + 0x7f], r3
vp_cpy_ub [r9 + 0x7f], r4
vp_cpy_ub [r10 + 0x7f], r5
vp_cpy_ub [r11 + 0x7f], r6
vp_cpy_ub [r12 + 0x7f], r7
vp_cpy_ub [r13 + 0x7f], r8
vp_cpy_ub [r14 + 0x7f], r9
vp_cpy_ub [r15 + 0x7f], r10
vp_cpy_ub [r0 + 0x7f], r11
vp_cpy_ub [r1 + 0x7f], r12
vp_cpy_ub [r2 + 0x7f], r13
vp_cpy_ub [r3 + 0x7f], r14
vp_cpy_ub [r4 + 0x7f], r15

vp_cpy_ub [r5 + 0x80], r0
vp_cpy_ub [r6 + 0x80], r1
vp_cpy_ub [r7 + 0x80], r2
vp_cpy_ub [r8 + 0x80], r3
vp_cpy_ub [r9 + 0x80], r4
vp_cpy_ub [r10 + 0x80], r5
vp_cpy_ub [r11 + 0x80], r6
vp_cpy_ub [r12 + 0x80], r7
vp_cpy_ub [r13 + 0x80], r8
vp_cpy_ub [r14 + 0x80], r9
vp_cpy_ub [r15 + 0x80], r10
vp_cpy_ub [r0 + 0x80], r11
vp_cpy_ub [r1 + 0x80], r12
vp_cpy_ub [r2 + 0x80], r13
vp_cpy_ub [r3 + 0x80], r14
vp_cpy_ub [r4 + 0x80], r15

vp_cpy_ub [r5 + 0x7fab1256], r0
vp_cpy_ub [r6 + 0x7fab1256], r1
vp_cpy_ub [r7 + 0x7fab1256], r2
vp_cpy_ub [r8 + 0x7fab1256], r3
vp_cpy_ub [r9 + 0x7fab1256], r4
vp_cpy_ub [r10 + 0x7fab1256], r5
vp_cpy_ub [r11 + 0x7fab1256], r6
vp_cpy_ub [r12 + 0x7fab1256], r7
vp_cpy_ub [r13 + 0x7fab1256], r8
vp_cpy_ub [r14 + 0x7fab1256], r9
vp_cpy_ub [r15 + 0x7fab1256], r10
vp_cpy_ub [r0 + 0x7fab1256], r11
vp_cpy_ub [r1 + 0x7fab1256], r12
vp_cpy_ub [r2 + 0x7fab1256], r13
vp_cpy_ub [r3 + 0x7fab1256], r14
vp_cpy_ub [r4 + 0x7fab1256], r15

;irus
vp_cpy_us [r5], r0
vp_cpy_us [r6], r1
vp_cpy_us [r7], r2
vp_cpy_us [r8], r3
vp_cpy_us [r9], r4
vp_cpy_us [r10], r5
vp_cpy_us [r11], r6
vp_cpy_us [r12], r7
vp_cpy_us [r13], r8
vp_cpy_us [r14], r9
vp_cpy_us [r15], r10
vp_cpy_us [r0], r11
vp_cpy_us [r1], r12
vp_cpy_us [r2], r13
vp_cpy_us [r3], r14
vp_cpy_us [r4], r15

vp_cpy_us [r5 + 0x7f], r0
vp_cpy_us [r6 + 0x7f], r1
vp_cpy_us [r7 + 0x7f], r2
vp_cpy_us [r8 + 0x7f], r3
vp_cpy_us [r9 + 0x7f], r4
vp_cpy_us [r10 + 0x7f], r5
vp_cpy_us [r11 + 0x7f], r6
vp_cpy_us [r12 + 0x7f], r7
vp_cpy_us [r13 + 0x7f], r8
vp_cpy_us [r14 + 0x7f], r9
vp_cpy_us [r15 + 0x7f], r10
vp_cpy_us [r0 + 0x7f], r11
vp_cpy_us [r1 + 0x7f], r12
vp_cpy_us [r2 + 0x7f], r13
vp_cpy_us [r3 + 0x7f], r14
vp_cpy_us [r4 + 0x7f], r15

vp_cpy_us [r5 + 0x80], r0
vp_cpy_us [r6 + 0x80], r1
vp_cpy_us [r7 + 0x80], r2
vp_cpy_us [r8 + 0x80], r3
vp_cpy_us [r9 + 0x80], r4
vp_cpy_us [r10 + 0x80], r5
vp_cpy_us [r11 + 0x80], r6
vp_cpy_us [r12 + 0x80], r7
vp_cpy_us [r13 + 0x80], r8
vp_cpy_us [r14 + 0x80], r9
vp_cpy_us [r15 + 0x80], r10
vp_cpy_us [r0 + 0x80], r11
vp_cpy_us [r1 + 0x80], r12
vp_cpy_us [r2 + 0x80], r13
vp_cpy_us [r3 + 0x80], r14
vp_cpy_us [r4 + 0x80], r15

vp_cpy_us [r5 + 0x7fab1256], r0
vp_cpy_us [r6 + 0x7fab1256], r1
vp_cpy_us [r7 + 0x7fab1256], r2
vp_cpy_us [r8 + 0x7fab1256], r3
vp_cpy_us [r9 + 0x7fab1256], r4
vp_cpy_us [r10 + 0x7fab1256], r5
vp_cpy_us [r11 + 0x7fab1256], r6
vp_cpy_us [r12 + 0x7fab1256], r7
vp_cpy_us [r13 + 0x7fab1256], r8
vp_cpy_us [r14 + 0x7fab1256], r9
vp_cpy_us [r15 + 0x7fab1256], r10
vp_cpy_us [r0 + 0x7fab1256], r11
vp_cpy_us [r1 + 0x7fab1256], r12
vp_cpy_us [r2 + 0x7fab1256], r13
vp_cpy_us [r3 + 0x7fab1256], r14
vp_cpy_us [r4 + 0x7fab1256], r15

;irui
vp_cpy_ui [r0], r0
vp_cpy_ui [r1], r1
vp_cpy_ui [r2], r2
vp_cpy_ui [r3], r3
vp_cpy_ui [r4], r4
vp_cpy_ui [r5], r5
vp_cpy_ui [r6], r6
vp_cpy_ui [r7], r7

vp_cpy_ui [r5], r0
vp_cpy_ui [r6], r1
vp_cpy_ui [r7], r2
vp_cpy_ui [r8], r3
vp_cpy_ui [r9], r4
vp_cpy_ui [r10], r5
vp_cpy_ui [r11], r6
vp_cpy_ui [r12], r7
vp_cpy_ui [r13], r8
vp_cpy_ui [r14], r9
vp_cpy_ui [r15], r10
vp_cpy_ui [r0], r11
vp_cpy_ui [r1], r12
vp_cpy_ui [r2], r13
vp_cpy_ui [r3], r14
vp_cpy_ui [r4], r15

vp_cpy_ui [r5 + 0x7f], r0
vp_cpy_ui [r6 + 0x7f], r1
vp_cpy_ui [r7 + 0x7f], r2
vp_cpy_ui [r8 + 0x7f], r3
vp_cpy_ui [r9 + 0x7f], r4
vp_cpy_ui [r10 + 0x7f], r5
vp_cpy_ui [r11 + 0x7f], r6
vp_cpy_ui [r12 + 0x7f], r7
vp_cpy_ui [r13 + 0x7f], r8
vp_cpy_ui [r14 + 0x7f], r9
vp_cpy_ui [r15 + 0x7f], r10
vp_cpy_ui [r0 + 0x7f], r11
vp_cpy_ui [r1 + 0x7f], r12
vp_cpy_ui [r2 + 0x7f], r13
vp_cpy_ui [r3 + 0x7f], r14
vp_cpy_ui [r4 + 0x7f], r15

vp_cpy_ui [r5 + 0x80], r0
vp_cpy_ui [r6 + 0x80], r1
vp_cpy_ui [r7 + 0x80], r2
vp_cpy_ui [r8 + 0x80], r3
vp_cpy_ui [r9 + 0x80], r4
vp_cpy_ui [r10 + 0x80], r5
vp_cpy_ui [r11 + 0x80], r6
vp_cpy_ui [r12 + 0x80], r7
vp_cpy_ui [r13 + 0x80], r8
vp_cpy_ui [r14 + 0x80], r9
vp_cpy_ui [r15 + 0x80], r10
vp_cpy_ui [r0 + 0x80], r11
vp_cpy_ui [r1 + 0x80], r12
vp_cpy_ui [r2 + 0x80], r13
vp_cpy_ui [r3 + 0x80], r14
vp_cpy_ui [r4 + 0x80], r15

vp_cpy_ui [r5 + 0x7fab1256], r0
vp_cpy_ui [r6 + 0x7fab1256], r1
vp_cpy_ui [r7 + 0x7fab1256], r2
vp_cpy_ui [r8 + 0x7fab1256], r3
vp_cpy_ui [r9 + 0x7fab1256], r4
vp_cpy_ui [r10 + 0x7fab1256], r5
vp_cpy_ui [r11 + 0x7fab1256], r6
vp_cpy_ui [r12 + 0x7fab1256], r7
vp_cpy_ui [r13 + 0x7fab1256], r8
vp_cpy_ui [r14 + 0x7fab1256], r9
vp_cpy_ui [r15 + 0x7fab1256], r10
vp_cpy_ui [r0 + 0x7fab1256], r11
vp_cpy_ui [r1 + 0x7fab1256], r12
vp_cpy_ui [r2 + 0x7fab1256], r13
vp_cpy_ui [r3 + 0x7fab1256], r14
vp_cpy_ui [r4 + 0x7fab1256], r15

;rib
vp_cpy_b r0, [r0]
vp_cpy_b r1, [r1]
vp_cpy_b r2, [r2]
vp_cpy_b r3, [r3]
vp_cpy_b r4, [r4]
vp_cpy_b r5, [r5]
vp_cpy_b r6, [r6]
vp_cpy_b r7, [r7]

vp_cpy_b r0, [r5]
vp_cpy_b r1, [r6]
vp_cpy_b r2, [r7]
vp_cpy_b r3, [r8]
vp_cpy_b r4, [r9]
vp_cpy_b r5, [r10]
vp_cpy_b r6, [r11]
vp_cpy_b r7, [r12]
vp_cpy_b r8, [r13]
vp_cpy_b r9, [r14]
vp_cpy_b r10, [r15]
vp_cpy_b r11, [r0]
vp_cpy_b r12, [r1]
vp_cpy_b r13, [r2]
vp_cpy_b r14, [r3]
vp_cpy_b r15, [r4]

vp_cpy_b r0, [r5 + 0x7f]
vp_cpy_b r1, [r6 + 0x7f]
vp_cpy_b r2, [r7 + 0x7f]
vp_cpy_b r3, [r8 + 0x7f]
vp_cpy_b r4, [r9 + 0x7f]
vp_cpy_b r5, [r10 + 0x7f]
vp_cpy_b r6, [r11 + 0x7f]
vp_cpy_b r7, [r12 + 0x7f]
vp_cpy_b r8, [r13 + 0x7f]
vp_cpy_b r9, [r14 + 0x7f]
vp_cpy_b r10, [r15 + 0x7f]
vp_cpy_b r11, [r0 + 0x7f]
vp_cpy_b r12, [r1 + 0x7f]
vp_cpy_b r13, [r2 + 0x7f]
vp_cpy_b r14, [r3 + 0x7f]
vp_cpy_b r15, [r4 + 0x7f]

vp_cpy_b r0, [r5 + 0x80]
vp_cpy_b r1, [r6 + 0x80]
vp_cpy_b r2, [r7 + 0x80]
vp_cpy_b r3, [r8 + 0x80]
vp_cpy_b r4, [r9 + 0x80]
vp_cpy_b r5, [r10 + 0x80]
vp_cpy_b r6, [r11 + 0x80]
vp_cpy_b r7, [r12 + 0x80]
vp_cpy_b r8, [r13 + 0x80]
vp_cpy_b r9, [r14 + 0x80]
vp_cpy_b r10, [r15 + 0x80]
vp_cpy_b r11, [r0 + 0x80]
vp_cpy_b r12, [r1 + 0x80]
vp_cpy_b r13, [r2 + 0x80]
vp_cpy_b r14, [r3 + 0x80]
vp_cpy_b r15, [r4 + 0x80]

vp_cpy_b r0, [r5 + 0x7fab1256]
vp_cpy_b r1, [r6 + 0x7fab1256]
vp_cpy_b r2, [r7 + 0x7fab1256]
vp_cpy_b r3, [r8 + 0x7fab1256]
vp_cpy_b r4, [r9 + 0x7fab1256]
vp_cpy_b r5, [r10 + 0x7fab1256]
vp_cpy_b r6, [r11 + 0x7fab1256]
vp_cpy_b r7, [r12 + 0x7fab1256]
vp_cpy_b r8, [r13 + 0x7fab1256]
vp_cpy_b r9, [r14 + 0x7fab1256]
vp_cpy_b r10, [r15 + 0x7fab1256]
vp_cpy_b r11, [r0 + 0x7fab1256]
vp_cpy_b r12, [r1 + 0x7fab1256]
vp_cpy_b r13, [r2 + 0x7fab1256]
vp_cpy_b r14, [r3 + 0x7fab1256]
vp_cpy_b r15, [r4 + 0x7fab1256]

;ris
vp_cpy_s r0, [r0]
vp_cpy_s r1, [r1]
vp_cpy_s r2, [r2]
vp_cpy_s r3, [r3]
vp_cpy_s r4, [r4]
vp_cpy_s r5, [r5]
vp_cpy_s r6, [r6]
vp_cpy_s r7, [r7]

vp_cpy_s r0, [r5]
vp_cpy_s r1, [r6]
vp_cpy_s r2, [r7]
vp_cpy_s r3, [r8]
vp_cpy_s r4, [r9]
vp_cpy_s r5, [r10]
vp_cpy_s r6, [r11]
vp_cpy_s r7, [r12]
vp_cpy_s r8, [r13]
vp_cpy_s r9, [r14]
vp_cpy_s r10, [r15]
vp_cpy_s r11, [r0]
vp_cpy_s r12, [r1]
vp_cpy_s r13, [r2]
vp_cpy_s r14, [r3]
vp_cpy_s r15, [r4]

vp_cpy_s r0, [r5 + 0x7f]
vp_cpy_s r1, [r6 + 0x7f]
vp_cpy_s r2, [r7 + 0x7f]
vp_cpy_s r3, [r8 + 0x7f]
vp_cpy_s r4, [r9 + 0x7f]
vp_cpy_s r5, [r10 + 0x7f]
vp_cpy_s r6, [r11 + 0x7f]
vp_cpy_s r7, [r12 + 0x7f]
vp_cpy_s r8, [r13 + 0x7f]
vp_cpy_s r9, [r14 + 0x7f]
vp_cpy_s r10, [r15 + 0x7f]
vp_cpy_s r11, [r0 + 0x7f]
vp_cpy_s r12, [r1 + 0x7f]
vp_cpy_s r13, [r2 + 0x7f]
vp_cpy_s r14, [r3 + 0x7f]
vp_cpy_s r15, [r4 + 0x7f]

vp_cpy_s r0, [r5 + 0x80]
vp_cpy_s r1, [r6 + 0x80]
vp_cpy_s r2, [r7 + 0x80]
vp_cpy_s r3, [r8 + 0x80]
vp_cpy_s r4, [r9 + 0x80]
vp_cpy_s r5, [r10 + 0x80]
vp_cpy_s r6, [r11 + 0x80]
vp_cpy_s r7, [r12 + 0x80]
vp_cpy_s r8, [r13 + 0x80]
vp_cpy_s r9, [r14 + 0x80]
vp_cpy_s r10, [r15 + 0x80]
vp_cpy_s r11, [r0 + 0x80]
vp_cpy_s r12, [r1 + 0x80]
vp_cpy_s r13, [r2 + 0x80]
vp_cpy_s r14, [r3 + 0x80]
vp_cpy_s r15, [r4 + 0x80]

vp_cpy_s r0, [r5 + 0x7fab1256]
vp_cpy_s r1, [r6 + 0x7fab1256]
vp_cpy_s r2, [r7 + 0x7fab1256]
vp_cpy_s r3, [r8 + 0x7fab1256]
vp_cpy_s r4, [r9 + 0x7fab1256]
vp_cpy_s r5, [r10 + 0x7fab1256]
vp_cpy_s r6, [r11 + 0x7fab1256]
vp_cpy_s r7, [r12 + 0x7fab1256]
vp_cpy_s r8, [r13 + 0x7fab1256]
vp_cpy_s r9, [r14 + 0x7fab1256]
vp_cpy_s r10, [r15 + 0x7fab1256]
vp_cpy_s r11, [r0 + 0x7fab1256]
vp_cpy_s r12, [r1 + 0x7fab1256]
vp_cpy_s r13, [r2 + 0x7fab1256]
vp_cpy_s r14, [r3 + 0x7fab1256]
vp_cpy_s r15, [r4 + 0x7fab1256]

;rii
vp_cpy_i r0, [r0]
vp_cpy_i r1, [r1]
vp_cpy_i r2, [r2]
vp_cpy_i r3, [r3]
vp_cpy_i r4, [r4]
vp_cpy_i r5, [r5]
vp_cpy_i r6, [r6]
vp_cpy_i r7, [r7]

vp_cpy_i r0, [r5]
vp_cpy_i r1, [r6]
vp_cpy_i r2, [r7]
vp_cpy_i r3, [r8]
vp_cpy_i r4, [r9]
vp_cpy_i r5, [r10]
vp_cpy_i r6, [r11]
vp_cpy_i r7, [r12]
vp_cpy_i r8, [r13]
vp_cpy_i r9, [r14]
vp_cpy_i r10, [r15]
vp_cpy_i r11, [r0]
vp_cpy_i r12, [r1]
vp_cpy_i r13, [r2]
vp_cpy_i r14, [r3]
vp_cpy_i r15, [r4]

vp_cpy_i r0, [r5 + 0x7f]
vp_cpy_i r1, [r6 + 0x7f]
vp_cpy_i r2, [r7 + 0x7f]
vp_cpy_i r3, [r8 + 0x7f]
vp_cpy_i r4, [r9 + 0x7f]
vp_cpy_i r5, [r10 + 0x7f]
vp_cpy_i r6, [r11 + 0x7f]
vp_cpy_i r7, [r12 + 0x7f]
vp_cpy_i r8, [r13 + 0x7f]
vp_cpy_i r9, [r14 + 0x7f]
vp_cpy_i r10, [r15 + 0x7f]
vp_cpy_i r11, [r0 + 0x7f]
vp_cpy_i r12, [r1 + 0x7f]
vp_cpy_i r13, [r2 + 0x7f]
vp_cpy_i r14, [r3 + 0x7f]
vp_cpy_i r15, [r4 + 0x7f]

vp_cpy_i r0, [r5 + 0x80]
vp_cpy_i r1, [r6 + 0x80]
vp_cpy_i r2, [r7 + 0x80]
vp_cpy_i r3, [r8 + 0x80]
vp_cpy_i r4, [r9 + 0x80]
vp_cpy_i r5, [r10 + 0x80]
vp_cpy_i r6, [r11 + 0x80]
vp_cpy_i r7, [r12 + 0x80]
vp_cpy_i r8, [r13 + 0x80]
vp_cpy_i r9, [r14 + 0x80]
vp_cpy_i r10, [r15 + 0x80]
vp_cpy_i r11, [r0 + 0x80]
vp_cpy_i r12, [r1 + 0x80]
vp_cpy_i r13, [r2 + 0x80]
vp_cpy_i r14, [r3 + 0x80]
vp_cpy_i r15, [r4 + 0x80]

vp_cpy_i r0, [r5 + 0x7fab1256]
vp_cpy_i r1, [r6 + 0x7fab1256]
vp_cpy_i r2, [r7 + 0x7fab1256]
vp_cpy_i r3, [r8 + 0x7fab1256]
vp_cpy_i r4, [r9 + 0x7fab1256]
vp_cpy_i r5, [r10 + 0x7fab1256]
vp_cpy_i r6, [r11 + 0x7fab1256]
vp_cpy_i r7, [r12 + 0x7fab1256]
vp_cpy_i r8, [r13 + 0x7fab1256]
vp_cpy_i r9, [r14 + 0x7fab1256]
vp_cpy_i r10, [r15 + 0x7fab1256]
vp_cpy_i r11, [r0 + 0x7fab1256]
vp_cpy_i r12, [r1 + 0x7fab1256]
vp_cpy_i r13, [r2 + 0x7fab1256]
vp_cpy_i r14, [r3 + 0x7fab1256]
vp_cpy_i r15, [r4 + 0x7fab1256]

;lea i
vp_lea  [r0], r0
vp_lea  [r1], r1
vp_lea  [r2], r2
vp_lea  [r3], r3
vp_lea  [r4], r4
vp_lea  [r5], r5
vp_lea  [r6], r6
vp_lea  [r7], r7

vp_lea  [r5], r0
vp_lea  [r6], r1
vp_lea  [r7], r2
vp_lea  [r8], r3
vp_lea  [r9], r4
vp_lea  [r10], r5
vp_lea  [r11], r6
vp_lea  [r12], r7
vp_lea  [r13], r8
vp_lea  [r14], r9
vp_lea  [r15], r10
vp_lea  [r0], r11
vp_lea  [r1], r12
vp_lea  [r2], r13
vp_lea  [r3], r14
vp_lea  [r4], r15

vp_lea  [r5 + 0x7f], r0
vp_lea  [r6 + 0x7f], r1
vp_lea  [r7 + 0x7f], r2
vp_lea  [r8 + 0x7f], r3
vp_lea  [r9 + 0x7f], r4
vp_lea  [r10 + 0x7f], r5
vp_lea  [r11 + 0x7f], r6
vp_lea  [r12 + 0x7f], r7
vp_lea  [r13 + 0x7f], r8
vp_lea  [r14 + 0x7f], r9
vp_lea  [r15 + 0x7f], r10
vp_lea  [r0 + 0x7f], r11
vp_lea  [r1 + 0x7f], r12
vp_lea  [r2 + 0x7f], r13
vp_lea  [r3 + 0x7f], r14
vp_lea  [r4 + 0x7f], r15

vp_lea  [r5 + 0x80], r0
vp_lea  [r6 + 0x80], r1
vp_lea  [r7 + 0x80], r2
vp_lea  [r8 + 0x80], r3
vp_lea  [r9 + 0x80], r4
vp_lea  [r10 + 0x80], r5
vp_lea  [r11 + 0x80], r6
vp_lea  [r12 + 0x80], r7
vp_lea  [r13 + 0x80], r8
vp_lea  [r14 + 0x80], r9
vp_lea  [r15 + 0x80], r10
vp_lea  [r0 + 0x80], r11
vp_lea  [r1 + 0x80], r12
vp_lea  [r2 + 0x80], r13
vp_lea  [r3 + 0x80], r14
vp_lea  [r4 + 0x80], r15

vp_lea  [r5 + 0x7fab1256], r0
vp_lea  [r6 + 0x7fab1256], r1
vp_lea  [r7 + 0x7fab1256], r2
vp_lea  [r8 + 0x7fab1256], r3
vp_lea  [r9 + 0x7fab1256], r4
vp_lea  [r10 + 0x7fab1256], r5
vp_lea  [r11 + 0x7fab1256], r6
vp_lea  [r12 + 0x7fab1256], r7
vp_lea  [r13 + 0x7fab1256], r8
vp_lea  [r14 + 0x7fab1256], r9
vp_lea  [r15 + 0x7fab1256], r10
vp_lea  [r0 + 0x7fab1256], r11
vp_lea  [r1 + 0x7fab1256], r12
vp_lea  [r2 + 0x7fab1256], r13
vp_lea  [r3 + 0x7fab1256], r14
vp_lea  [r4 + 0x7fab1256], r15

;rr
vp_cpy r0, r3
vp_cpy r1, r4
vp_cpy r2, r5
vp_cpy r3, r6
vp_cpy r4, r7
vp_cpy r5, r8
vp_cpy r6, r9
vp_cpy r7, r10
vp_cpy r8, r11
vp_cpy r9, r12
vp_cpy r10, r13
vp_cpy r11, r14
vp_cpy r12, r15
vp_cpy r13, r0
vp_cpy r14, r1
vp_cpy r15, r2

vp_add r0, r3
vp_add r1, r4
vp_add r2, r5
vp_add r3, r6
vp_add r4, r7
vp_add r5, r8
vp_add r6, r9
vp_add r7, r10
vp_add r8, r11
vp_add r9, r12
vp_add r10, r13
vp_add r11, r14
vp_add r12, r15
vp_add r13, r0
vp_add r14, r1
vp_add r15, r2

vp_sub r0, r3
vp_sub r1, r4
vp_sub r2, r5
vp_sub r3, r6
vp_sub r4, r7
vp_sub r5, r8
vp_sub r6, r9
vp_sub r7, r10
vp_sub r8, r11
vp_sub r9, r12
vp_sub r10, r13
vp_sub r11, r14
vp_sub r12, r15
vp_sub r13, r0
vp_sub r14, r1
vp_sub r15, r2

vp_cmp r0, r3
vp_cmp r1, r4
vp_cmp r2, r5
vp_cmp r3, r6
vp_cmp r4, r7
vp_cmp r5, r8
vp_cmp r6, r9
vp_cmp r7, r10
vp_cmp r8, r11
vp_cmp r9, r12
vp_cmp r10, r13
vp_cmp r11, r14
vp_cmp r12, r15
vp_cmp r13, r0
vp_cmp r14, r1
vp_cmp r15, r2

vp_and r0, r3
vp_and r1, r4
vp_and r2, r5
vp_and r3, r6
vp_and r4, r7
vp_and r5, r8
vp_and r6, r9
vp_and r7, r10
vp_and r8, r11
vp_and r9, r12
vp_and r10, r13
vp_and r11, r14
vp_and r12, r15
vp_and r13, r0
vp_and r14, r1
vp_and r15, r2

vp_or r0, r3
vp_or r1, r4
vp_or r2, r5
vp_or r3, r6
vp_or r4, r7
vp_or r5, r8
vp_or r6, r9
vp_or r7, r10
vp_or r8, r11
vp_or r9, r12
vp_or r10, r13
vp_or r11, r14
vp_or r12, r15
vp_or r13, r0
vp_or r14, r1
vp_or r15, r2

vp_xor r0, r3
vp_xor r1, r4
vp_xor r2, r5
vp_xor r3, r6
vp_xor r4, r7
vp_xor r5, r8
vp_xor r6, r9
vp_xor r7, r10
vp_xor r8, r11
vp_xor r9, r12
vp_xor r10, r13
vp_xor r11, r14
vp_xor r12, r15
vp_xor r13, r0
vp_xor r14, r1
vp_xor r15, r2

vp_mul r0, r3
vp_mul r1, r4
vp_mul r2, r5
vp_mul r3, r6
vp_mul r4, r7
vp_mul r5, r8
vp_mul r6, r9
vp_mul r7, r10
vp_mul r8, r11
vp_mul r9, r12
vp_mul r10, r13
vp_mul r11, r14
vp_mul r12, r15
vp_mul r13, r0
vp_mul r14, r1
vp_mul r15, r2

vp_xchg r1, r4
vp_xchg r2, r5
vp_xchg r3, r6
vp_xchg r4, r7
vp_xchg r5, r8
vp_xchg r6, r9
vp_xchg r7, r10
vp_xchg r8, r11
vp_xchg r9, r12
vp_xchg r10, r13
vp_xchg r11, r1
vp_xchg r11, r14
vp_xchg r12, r15
vp_xchg r14, r1
vp_xchg r15, r2
vp_xchg r2, r15
vp_xchg r1, r2
vp_xchg r2, r3
vp_xchg r3, r4
vp_xchg r4, r5
vp_xchg r5, r6
vp_xchg r6, r7
vp_xchg r0, r1
vp_xchg r0, r2
vp_xchg r0, r3
vp_xchg r0, r4
vp_xchg r0, r5
vp_xchg r0, r6
vp_xchg r0, r7
vp_xchg r0, r8
vp_xchg r0, r9
vp_xchg r0, r10
vp_xchg r0, r11
vp_xchg r0, r12
vp_xchg r0, r13
vp_xchg r0, r14
vp_xchg r0, r15

;dr
vp_cpy [r0 + r1], r0
vp_cpy [r0 + r1], r1
vp_cpy [r0 + r1], r5
vp_cpy [r0 + r1], r6
vp_cpy [r0 + r1], r11
vp_cpy [r0 + r1], r12
vp_cpy [r0 + r1], r13
vp_cpy [r0 + r1], r14
vp_cpy [r0 + r1], r15
vp_cpy [r1 + r1], r0
vp_cpy [r2 + r1], r1
vp_cpy [r3 + r15], r2
vp_cpy [r5 + r13], r4
vp_cpy [r6 + r12], r5
vp_cpy [r7 + r11], r6
vp_cpy [r8 + r10], r7
vp_cpy [r9 + r9], r8
vp_cpy [r10 + r8], r9
vp_cpy [r11 + r7], r10
vp_cpy [r12 + r6], r11
vp_cpy [r13 + r5], r12
vp_cpy [r14 + r3], r13
vp_cpy [r15 + r2], r14
vp_cpy [r12 + r1], r15
vp_cpy [r1 + r13], r0
vp_cpy [r13 + r1], r0

vp_add [r0 + r1], r0
vp_add [r0 + r1], r1
vp_add [r0 + r1], r5
vp_add [r0 + r1], r6
vp_add [r0 + r1], r11
vp_add [r0 + r1], r12
vp_add [r0 + r1], r13
vp_add [r0 + r1], r14
vp_add [r0 + r1], r15
vp_add [r1 + r1], r0
vp_add [r2 + r1], r1
vp_add [r3 + r15], r2
vp_add [r5 + r13], r4
vp_add [r6 + r12], r5
vp_add [r7 + r11], r6
vp_add [r8 + r10], r7
vp_add [r9 + r9], r8
vp_add [r10 + r8], r9
vp_add [r11 + r7], r10
vp_add [r12 + r6], r11
vp_add [r13 + r5], r12
vp_add [r14 + r3], r13
vp_add [r15 + r2], r14
vp_add [r12 + r1], r15
vp_add [r1 + r13], r0
vp_add [r13 + r1], r0

vp_sub [r0 + r1], r0
vp_sub [r0 + r1], r1
vp_sub [r0 + r1], r5
vp_sub [r0 + r1], r6
vp_sub [r0 + r1], r11
vp_sub [r0 + r1], r12
vp_sub [r0 + r1], r13
vp_sub [r0 + r1], r14
vp_sub [r0 + r1], r15
vp_sub [r1 + r1], r0
vp_sub [r2 + r1], r1
vp_sub [r3 + r15], r2
vp_sub [r5 + r13], r4
vp_sub [r6 + r12], r5
vp_sub [r7 + r11], r6
vp_sub [r8 + r10], r7
vp_sub [r9 + r9], r8
vp_sub [r10 + r8], r9
vp_sub [r11 + r7], r10
vp_sub [r12 + r6], r11
vp_sub [r13 + r5], r12
vp_sub [r14 + r3], r13
vp_sub [r15 + r2], r14
vp_sub [r12 + r1], r15
vp_sub [r1 + r13], r0
vp_sub [r13 + r1], r0

vp_cmp [r0 + r1], r0
vp_cmp [r0 + r1], r1
vp_cmp [r0 + r1], r5
vp_cmp [r0 + r1], r6
vp_cmp [r0 + r1], r11
vp_cmp [r0 + r1], r12
vp_cmp [r0 + r1], r13
vp_cmp [r0 + r1], r14
vp_cmp [r0 + r1], r15
vp_cmp [r1 + r1], r0
vp_cmp [r2 + r1], r1
vp_cmp [r3 + r15], r2
vp_cmp [r5 + r13], r4
vp_cmp [r6 + r12], r5
vp_cmp [r7 + r11], r6
vp_cmp [r8 + r10], r7
vp_cmp [r9 + r9], r8
vp_cmp [r10 + r8], r9
vp_cmp [r11 + r7], r10
vp_cmp [r12 + r6], r11
vp_cmp [r13 + r5], r12
vp_cmp [r14 + r3], r13
vp_cmp [r15 + r2], r14
vp_cmp [r12 + r1], r15
vp_cmp [r1 + r13], r0
vp_cmp [r13 + r1], r0

vp_and [r0 + r1], r0
vp_and [r0 + r1], r1
vp_and [r0 + r1], r5
vp_and [r0 + r1], r6
vp_and [r0 + r1], r11
vp_and [r0 + r1], r12
vp_and [r0 + r1], r13
vp_and [r0 + r1], r14
vp_and [r0 + r1], r15
vp_and [r1 + r1], r0
vp_and [r2 + r1], r1
vp_and [r3 + r15], r2
vp_and [r5 + r13], r4
vp_and [r6 + r12], r5
vp_and [r7 + r11], r6
vp_and [r8 + r10], r7
vp_and [r9 + r9], r8
vp_and [r10 + r8], r9
vp_and [r11 + r7], r10
vp_and [r12 + r6], r11
vp_and [r13 + r5], r12
vp_and [r14 + r3], r13
vp_and [r15 + r2], r14
vp_and [r12 + r1], r15
vp_and [r1 + r13], r0
vp_and [r13 + r1], r0

vp_or [r0 + r1], r0
vp_or [r0 + r1], r1
vp_or [r0 + r1], r5
vp_or [r0 + r1], r6
vp_or [r0 + r1], r11
vp_or [r0 + r1], r12
vp_or [r0 + r1], r13
vp_or [r0 + r1], r14
vp_or [r0 + r1], r15
vp_or [r1 + r1], r0
vp_or [r2 + r1], r1
vp_or [r3 + r15], r2
vp_or [r5 + r13], r4
vp_or [r6 + r12], r5
vp_or [r7 + r11], r6
vp_or [r8 + r10], r7
vp_or [r9 + r9], r8
vp_or [r10 + r8], r9
vp_or [r11 + r7], r10
vp_or [r12 + r6], r11
vp_or [r13 + r5], r12
vp_or [r14 + r3], r13
vp_or [r15 + r2], r14
vp_or [r12 + r1], r15
vp_or [r1 + r13], r0
vp_or [r13 + r1], r0

vp_xor [r0 + r1], r0
vp_xor [r0 + r1], r1
vp_xor [r0 + r1], r5
vp_xor [r0 + r1], r6
vp_xor [r0 + r1], r11
vp_xor [r0 + r1], r12
vp_xor [r0 + r1], r13
vp_xor [r0 + r1], r14
vp_xor [r0 + r1], r15
vp_xor [r1 + r1], r0
vp_xor [r2 + r1], r1
vp_xor [r3 + r15], r2
vp_xor [r5 + r13], r4
vp_xor [r6 + r12], r5
vp_xor [r7 + r11], r6
vp_xor [r8 + r10], r7
vp_xor [r9 + r9], r8
vp_xor [r10 + r8], r9
vp_xor [r11 + r7], r10
vp_xor [r12 + r6], r11
vp_xor [r13 + r5], r12
vp_xor [r14 + r3], r13
vp_xor [r15 + r2], r14
vp_xor [r12 + r1], r15
vp_xor [r1 + r13], r0
vp_xor [r13 + r1], r0

vp_mul [r0 + r1], r0
vp_mul [r0 + r1], r1
vp_mul [r0 + r1], r5
vp_mul [r0 + r1], r6
vp_mul [r0 + r1], r11
vp_mul [r0 + r1], r12
vp_mul [r0 + r1], r13
vp_mul [r0 + r1], r14
vp_mul [r0 + r1], r15
vp_mul [r1 + r1], r0
vp_mul [r2 + r1], r1
vp_mul [r3 + r15], r2
vp_mul [r5 + r13], r4
vp_mul [r6 + r12], r5
vp_mul [r7 + r11], r6
vp_mul [r8 + r10], r7
vp_mul [r9 + r9], r8
vp_mul [r10 + r8], r9
vp_mul [r11 + r7], r10
vp_mul [r12 + r6], r11
vp_mul [r13 + r5], r12
vp_mul [r14 + r3], r13
vp_mul [r15 + r2], r14
vp_mul [r12 + r1], r15
vp_mul [r1 + r13], r0
vp_mul [r13 + r1], r0

def_func_end
