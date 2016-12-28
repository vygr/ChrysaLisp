%include "inc/func.ninc"

def_func test

label2:
;push and pop
vp_push r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15
vp_pop r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15

;const/reg
vp_cpy 0, r0
vp_cpy 1, r0
vp_cpy 0x7f, r0
vp_cpy 0x80, r0
vp_cpy 0x7fffffff, r0
vp_cpy 0x80000000, r0
vp_cpy 0xffffffff, r0
vp_cpy 0x100000000, r0
vp_cpy -1, r0
vp_cpy -0x80, r0
vp_cpy -0x81, r0
vp_cpy -0x80000000, r0
vp_cpy -0x80000001, r0
vp_cpy -0xffffffff, r0
vp_cpy -0x100000000, r0
vp_cpy 1, r8
vp_cpy 0x7f, r8
vp_cpy 0x80, r8
vp_cpy 0x7fffffff, r8
vp_cpy 0x80000000, r8
vp_cpy 0xffffffff, r8
vp_cpy 0x100000000, r8
vp_cpy -1, r8
vp_cpy -0x80, r8
vp_cpy -0x81, r8
vp_cpy -0x80000000, r8
vp_cpy -0x80000001, r8
vp_cpy -0xffffffff, r8
vp_cpy -0x100000000, r8

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

vp_mul -1, r0
vp_mul 0x7f, r1
vp_mul 0x7f, r2
vp_mul 0x7f, r3
vp_mul 0x7f, r4
vp_mul 0x7f, r5
vp_mul 0x7f, r6
vp_mul 0x7f, r7
vp_mul 0x7f, r8
vp_mul 0x7f, r9
vp_mul -1, r10
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

;ri
vp_cpy r0, [r0]
vp_cpy r1, [r1]
vp_cpy r2, [r2]
vp_cpy r3, [r3]
vp_cpy r4, [r4]
vp_cpy r5, [r5]
vp_cpy r6, [r6]
vp_cpy r7, [r7]

vp_cpy r0, [r5]
vp_cpy r1, [r6]
vp_cpy r2, [r7]
vp_cpy r3, [r8]
vp_cpy r4, [r9]
vp_cpy r5, [r10]
vp_cpy r6, [r11]
vp_cpy r7, [r12]
vp_cpy r8, [r13]
vp_cpy r9, [r14]
vp_cpy r10, [r15]
vp_cpy r11, [r0]
vp_cpy r12, [r1]
vp_cpy r13, [r2]
vp_cpy r14, [r3]
vp_cpy r15, [r4]

vp_cpy r0, [r5 + 0x7f]
vp_cpy r1, [r6 + 0x7f]
vp_cpy r2, [r7 + 0x7f]
vp_cpy r3, [r8 + 0x7f]
vp_cpy r4, [r9 + 0x7f]
vp_cpy r5, [r10 + 0x7f]
vp_cpy r6, [r11 + 0x7f]
vp_cpy r7, [r12 + 0x7f]
vp_cpy r8, [r13 + 0x7f]
vp_cpy r9, [r14 + 0x7f]
vp_cpy r10, [r15 + 0x7f]
vp_cpy r11, [r0 + 0x7f]
vp_cpy r12, [r1 + 0x7f]
vp_cpy r13, [r2 + 0x7f]
vp_cpy r14, [r3 + 0x7f]
vp_cpy r15, [r4 + 0x7f]

vp_cpy r0, [r5 + 0x80]
vp_cpy r1, [r6 + 0x80]
vp_cpy r2, [r7 + 0x80]
vp_cpy r3, [r8 + 0x80]
vp_cpy r4, [r9 + 0x80]
vp_cpy r5, [r10 + 0x80]
vp_cpy r6, [r11 + 0x80]
vp_cpy r7, [r12 + 0x80]
vp_cpy r8, [r13 + 0x80]
vp_cpy r9, [r14 + 0x80]
vp_cpy r10, [r15 + 0x80]
vp_cpy r11, [r0 + 0x80]
vp_cpy r12, [r1 + 0x80]
vp_cpy r13, [r2 + 0x80]
vp_cpy r14, [r3 + 0x80]
vp_cpy r15, [r4 + 0x80]

vp_cpy r0, [r5 + 0x7fab1256]
vp_cpy r1, [r6 + 0x7fab1256]
vp_cpy r2, [r7 + 0x7fab1256]
vp_cpy r3, [r8 + 0x7fab1256]
vp_cpy r4, [r9 + 0x7fab1256]
vp_cpy r5, [r10 + 0x7fab1256]
vp_cpy r6, [r11 + 0x7fab1256]
vp_cpy r7, [r12 + 0x7fab1256]
vp_cpy r8, [r13 + 0x7fab1256]
vp_cpy r9, [r14 + 0x7fab1256]
vp_cpy r10, [r15 + 0x7fab1256]
vp_cpy r11, [r0 + 0x7fab1256]
vp_cpy r12, [r1 + 0x7fab1256]
vp_cpy r13, [r2 + 0x7fab1256]
vp_cpy r14, [r3 + 0x7fab1256]
vp_cpy r15, [r4 + 0x7fab1256]

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

;dr bsubus
vp_cpy_b [r0 + r1], r0
vp_cpy_b [r0 + r1], r1
vp_cpy_b [r0 + r1], r5
vp_cpy_b [r0 + r1], r6
vp_cpy_b [r0 + r1], r11
vp_cpy_b [r0 + r1], r12
vp_cpy_b [r0 + r1], r13
vp_cpy_b [r0 + r1], r14
vp_cpy_b [r0 + r1], r15
vp_cpy_b [r1 + r1], r0
vp_cpy_b [r2 + r1], r1
vp_cpy_b [r3 + r15], r2
vp_cpy_b [r5 + r13], r4
vp_cpy_b [r6 + r12], r5
vp_cpy_b [r7 + r11], r6
vp_cpy_b [r8 + r10], r7
vp_cpy_b [r9 + r9], r8
vp_cpy_b [r10 + r8], r9
vp_cpy_b [r11 + r7], r10
vp_cpy_b [r12 + r6], r11
vp_cpy_b [r13 + r5], r12
vp_cpy_b [r14 + r3], r13
vp_cpy_b [r15 + r2], r14
vp_cpy_b [r12 + r1], r15
vp_cpy_b [r1 + r13], r0
vp_cpy_b [r13 + r1], r0

vp_cpy_ub [r0 + r1], r0
vp_cpy_ub [r0 + r1], r1
vp_cpy_ub [r0 + r1], r5
vp_cpy_ub [r0 + r1], r6
vp_cpy_ub [r0 + r1], r11
vp_cpy_ub [r0 + r1], r12
vp_cpy_ub [r0 + r1], r13
vp_cpy_ub [r0 + r1], r14
vp_cpy_ub [r0 + r1], r15
vp_cpy_ub [r1 + r1], r0
vp_cpy_ub [r2 + r1], r1
vp_cpy_ub [r3 + r15], r2
vp_cpy_ub [r5 + r13], r4
vp_cpy_ub [r6 + r12], r5
vp_cpy_ub [r7 + r11], r6
vp_cpy_ub [r8 + r10], r7
vp_cpy_ub [r9 + r9], r8
vp_cpy_ub [r10 + r8], r9
vp_cpy_ub [r11 + r7], r10
vp_cpy_ub [r12 + r6], r11
vp_cpy_ub [r13 + r5], r12
vp_cpy_ub [r14 + r3], r13
vp_cpy_ub [r15 + r2], r14
vp_cpy_ub [r12 + r1], r15
vp_cpy_ub [r1 + r13], r0
vp_cpy_ub [r13 + r1], r0

vp_cpy_s [r0 + r1], r0
vp_cpy_s [r0 + r1], r1
vp_cpy_s [r0 + r1], r5
vp_cpy_s [r0 + r1], r6
vp_cpy_s [r0 + r1], r11
vp_cpy_s [r0 + r1], r12
vp_cpy_s [r0 + r1], r13
vp_cpy_s [r0 + r1], r14
vp_cpy_s [r0 + r1], r15
vp_cpy_s [r1 + r1], r0
vp_cpy_s [r2 + r1], r1
vp_cpy_s [r3 + r15], r2
vp_cpy_s [r5 + r13], r4
vp_cpy_s [r6 + r12], r5
vp_cpy_s [r7 + r11], r6
vp_cpy_s [r8 + r10], r7
vp_cpy_s [r9 + r9], r8
vp_cpy_s [r10 + r8], r9
vp_cpy_s [r11 + r7], r10
vp_cpy_s [r12 + r6], r11
vp_cpy_s [r13 + r5], r12
vp_cpy_s [r14 + r3], r13
vp_cpy_s [r15 + r2], r14
vp_cpy_s [r12 + r1], r15
vp_cpy_s [r1 + r13], r0
vp_cpy_s [r13 + r1], r0

vp_cpy_us [r0 + r1], r0
vp_cpy_us [r0 + r1], r1
vp_cpy_us [r0 + r1], r5
vp_cpy_us [r0 + r1], r6
vp_cpy_us [r0 + r1], r11
vp_cpy_us [r0 + r1], r12
vp_cpy_us [r0 + r1], r13
vp_cpy_us [r0 + r1], r14
vp_cpy_us [r0 + r1], r15
vp_cpy_us [r1 + r1], r0
vp_cpy_us [r2 + r1], r1
vp_cpy_us [r3 + r15], r2
vp_cpy_us [r5 + r13], r4
vp_cpy_us [r6 + r12], r5
vp_cpy_us [r7 + r11], r6
vp_cpy_us [r8 + r10], r7
vp_cpy_us [r9 + r9], r8
vp_cpy_us [r10 + r8], r9
vp_cpy_us [r11 + r7], r10
vp_cpy_us [r12 + r6], r11
vp_cpy_us [r13 + r5], r12
vp_cpy_us [r14 + r3], r13
vp_cpy_us [r15 + r2], r14
vp_cpy_us [r12 + r1], r15
vp_cpy_us [r1 + r13], r0
vp_cpy_us [r13 + r1], r0

;dr iui
vp_cpy_i [r0 + r1], r0
vp_cpy_i [r0 + r1], r1
vp_cpy_i [r0 + r1], r5
vp_cpy_i [r0 + r1], r6
vp_cpy_i [r0 + r1], r11
vp_cpy_i [r0 + r1], r12
vp_cpy_i [r0 + r1], r13
vp_cpy_i [r0 + r1], r14
vp_cpy_i [r0 + r1], r15
vp_cpy_i [r1 + r1], r0
vp_cpy_i [r2 + r1], r1
vp_cpy_i [r3 + r15], r2
vp_cpy_i [r5 + r13], r4
vp_cpy_i [r6 + r12], r5
vp_cpy_i [r7 + r11], r6
vp_cpy_i [r8 + r10], r7
vp_cpy_i [r9 + r9], r8
vp_cpy_i [r10 + r8], r9
vp_cpy_i [r11 + r7], r10
vp_cpy_i [r12 + r6], r11
vp_cpy_i [r13 + r5], r12
vp_cpy_i [r14 + r3], r13
vp_cpy_i [r15 + r2], r14
vp_cpy_i [r12 + r1], r15
vp_cpy_i [r1 + r13], r0
vp_cpy_i [r13 + r1], r0

vp_cpy_ui [r0 + r1], r0
vp_cpy_ui [r0 + r1], r1
vp_cpy_ui [r0 + r1], r5
vp_cpy_ui [r0 + r1], r6
vp_cpy_ui [r0 + r1], r11
vp_cpy_ui [r0 + r1], r12
vp_cpy_ui [r0 + r1], r13
vp_cpy_ui [r0 + r1], r14
vp_cpy_ui [r0 + r1], r15
vp_cpy_ui [r1 + r1], r0
vp_cpy_ui [r2 + r1], r1
vp_cpy_ui [r3 + r15], r2
vp_cpy_ui [r5 + r13], r4
vp_cpy_ui [r6 + r12], r5
vp_cpy_ui [r7 + r11], r6
vp_cpy_ui [r8 + r10], r7
vp_cpy_ui [r9 + r9], r8
vp_cpy_ui [r10 + r8], r9
vp_cpy_ui [r11 + r7], r10
vp_cpy_ui [r12 + r6], r11
vp_cpy_ui [r13 + r5], r12
vp_cpy_ui [r14 + r3], r13
vp_cpy_ui [r15 + r2], r14
vp_cpy_ui [r12 + r1], r15
vp_cpy_ui [r1 + r13], r0
vp_cpy_ui [r13 + r1], r0

;rd bsi
vp_cpy_b r0, [r0 + r1]
vp_cpy_b r1, [r0 + r1]
vp_cpy_b r5, [r0 + r1]
vp_cpy_b r6, [r0 + r1]
vp_cpy_b r11, [r0 + r1]
vp_cpy_b r12, [r0 + r1]
vp_cpy_b r13, [r0 + r1]
vp_cpy_b r14, [r0 + r1]
vp_cpy_b r15, [r0 + r1]
vp_cpy_b r0, [r1 + r1]
vp_cpy_b r1, [r2 + r1]
vp_cpy_b r2, [r3 + r15]
vp_cpy_b r4, [r5 + r13]
vp_cpy_b r5, [r6 + r12]
vp_cpy_b r6, [r7 + r11]
vp_cpy_b r7, [r8 + r10]
vp_cpy_b r8, [r9 + r9]
vp_cpy_b r9, [r10 + r8]
vp_cpy_b r10, [r11 + r7]
vp_cpy_b r11, [r12 + r6]
vp_cpy_b r12, [r13 + r5]
vp_cpy_b r13, [r14 + r3]
vp_cpy_b r14, [r15 + r2]
vp_cpy_b r15, [r12 + r1]
vp_cpy_b r0, [r1 + r13]
vp_cpy_b r0, [r13 + r1]

vp_cpy_s r0, [r0 + r1]
vp_cpy_s r1, [r0 + r1]
vp_cpy_s r5, [r0 + r1]
vp_cpy_s r6, [r0 + r1]
vp_cpy_s r11, [r0 + r1]
vp_cpy_s r12, [r0 + r1]
vp_cpy_s r13, [r0 + r1]
vp_cpy_s r14, [r0 + r1]
vp_cpy_s r15, [r0 + r1]
vp_cpy_s r0, [r1 + r1]
vp_cpy_s r1, [r2 + r1]
vp_cpy_s r2, [r3 + r15]
vp_cpy_s r4, [r5 + r13]
vp_cpy_s r5, [r6 + r12]
vp_cpy_s r6, [r7 + r11]
vp_cpy_s r7, [r8 + r10]
vp_cpy_s r8, [r9 + r9]
vp_cpy_s r9, [r10 + r8]
vp_cpy_s r10, [r11 + r7]
vp_cpy_s r11, [r12 + r6]
vp_cpy_s r12, [r13 + r5]
vp_cpy_s r13, [r14 + r3]
vp_cpy_s r14, [r15 + r2]
vp_cpy_s r15, [r12 + r1]
vp_cpy_s r0, [r1 + r13]
vp_cpy_s r0, [r13 + r1]

vp_cpy_i r0, [r0 + r1]
vp_cpy_i r1, [r0 + r1]
vp_cpy_i r5, [r0 + r1]
vp_cpy_i r6, [r0 + r1]
vp_cpy_i r11, [r0 + r1]
vp_cpy_i r12, [r0 + r1]
vp_cpy_i r13, [r0 + r1]
vp_cpy_i r14, [r0 + r1]
vp_cpy_i r15, [r0 + r1]
vp_cpy_i r0, [r1 + r1]
vp_cpy_i r1, [r2 + r1]
vp_cpy_i r2, [r3 + r15]
vp_cpy_i r4, [r5 + r13]
vp_cpy_i r5, [r6 + r12]
vp_cpy_i r6, [r7 + r11]
vp_cpy_i r7, [r8 + r10]
vp_cpy_i r8, [r9 + r9]
vp_cpy_i r9, [r10 + r8]
vp_cpy_i r10, [r11 + r7]
vp_cpy_i r11, [r12 + r6]
vp_cpy_i r12, [r13 + r5]
vp_cpy_i r13, [r14 + r3]
vp_cpy_i r14, [r15 + r2]
vp_cpy_i r15, [r12 + r1]
vp_cpy_i r0, [r1 + r13]
vp_cpy_i r0, [r13 + r1]

vp_cpy r0, [r0 + r1]
vp_cpy r1, [r0 + r1]
vp_cpy r5, [r0 + r1]
vp_cpy r6, [r0 + r1]
vp_cpy r11, [r0 + r1]
vp_cpy r12, [r0 + r1]
vp_cpy r13, [r0 + r1]
vp_cpy r14, [r0 + r1]
vp_cpy r15, [r0 + r1]
vp_cpy r0, [r1 + r1]
vp_cpy r1, [r2 + r1]
vp_cpy r2, [r3 + r15]
vp_cpy r4, [r5 + r13]
vp_cpy r5, [r6 + r12]
vp_cpy r6, [r7 + r11]
vp_cpy r7, [r8 + r10]
vp_cpy r8, [r9 + r9]
vp_cpy r9, [r10 + r8]
vp_cpy r10, [r11 + r7]
vp_cpy r11, [r12 + r6]
vp_cpy r12, [r13 + r5]
vp_cpy r13, [r14 + r3]
vp_cpy r14, [r15 + r2]
vp_cpy r15, [r12 + r1]
vp_cpy r0, [r1 + r13]
vp_cpy r0, [r13 + r1]

;lea d
vp_lea [r0 + r1], r0
vp_lea [r0 + r1], r1
vp_lea [r0 + r1], r5
vp_lea [r13 + r1], r6
vp_lea [r0 + r1], r11
vp_lea [r0 + r1], r12
vp_lea [r0 + r1], r13
vp_lea [r5 + r1], r14
vp_lea [r0 + r1], r15
vp_lea [r1 + r1], r0
vp_lea [r2 + r1], r1
vp_lea [r3 + r15], r2
vp_lea [r5 + r13], r4
vp_lea [r6 + r12], r5
vp_lea [r7 + r11], r6
vp_lea [r8 + r10], r7
vp_lea [r9 + r9], r8
vp_lea [r10 + r8], r9
vp_lea [r11 + r7], r10
vp_lea [r12 + r6], r11
vp_lea [r13 + r5], r12
vp_lea [r14 + r3], r13
vp_lea [r15 + r2], r14
vp_lea [r12 + r1], r15
vp_lea [r1 + r13], r0
vp_lea [r13 + r1], r0

;p modes
vp_lea_p label1, r0
vp_lea_p label1, r1
vp_lea_p label1, r2
vp_lea_p label1, r3
vp_lea_p label1, r4
vp_lea_p label1, r5
vp_lea_p label1, r6
vp_lea_p label1, r7
vp_lea_p label1, r8
vp_lea_p label1, r9
vp_lea_p label1, r10
vp_lea_p label1, r11
vp_lea_p label1, r12
vp_lea_p label1, r13
vp_lea_p label1, r14
vp_lea_p label1, r15

vp_cpy_p label1, r0
vp_cpy_p label1, r1
vp_cpy_p label1, r2
vp_cpy_p label1, r3
vp_cpy_p label1, r4
vp_cpy_p label1, r5
vp_cpy_p label1, r6
vp_cpy_p label1, r7
vp_cpy_p label1, r8
vp_cpy_p label1, r9
vp_cpy_p label1, r10
vp_cpy_p label1, r11
vp_cpy_p label1, r12
vp_cpy_p label1, r13
vp_cpy_p label1, r14
vp_cpy_p label1, r15

vp_cpy_p r0, label1
vp_cpy_p r1, label1
vp_cpy_p r2, label1
vp_cpy_p r3, label1
vp_cpy_p r4, label1
vp_cpy_p r5, label1
vp_cpy_p r6, label1
vp_cpy_p r7, label1
vp_cpy_p r8, label1
vp_cpy_p r9, label1
vp_cpy_p r10, label1
vp_cpy_p r11, label1
vp_cpy_p r12, label1
vp_cpy_p r13, label1
vp_cpy_p r14, label1
vp_cpy_p r15, label1

;call/jmp
vp_call label3
vp_jmp label3
vp_call label2
vp_jmp label2
vp_call_p label2
vp_jmp_p label2
label3:

vp_call r0
vp_call r1
vp_call r2
vp_call r3
vp_call r4
vp_call r5
vp_call r6
vp_call r7
vp_call r8
vp_call r9
vp_call r10
vp_call r11
vp_call r12
vp_call r13
vp_call r14
vp_call r15

vp_jmp r0
vp_jmp r1
vp_jmp r2
vp_jmp r3
vp_jmp r4
vp_jmp r5
vp_jmp r6
vp_jmp r7
vp_jmp r8
vp_jmp r9
vp_jmp r10
vp_jmp r11
vp_jmp r12
vp_jmp r13
vp_jmp r14
vp_jmp r15

vp_call [r0 + r1]
vp_call [r2 + r1]
vp_call [r3 + r15]
vp_call [r5 + r13]
vp_call [r6 + r12]
vp_call [r7 + r11]
vp_call [r8 + r10]
vp_call [r9 + r9]
vp_call [r10 + r8]
vp_call [r11 + r7]
vp_call [r12 + r6]
vp_call [r13 + r5]
vp_call [r14 + r3]
vp_call [r15 + r2]
vp_call [r12 + r1]
vp_call [r1 + r13]
vp_call [r13 + r1]

vp_jmp [r0 + r1]
vp_jmp [r2 + r1]
vp_jmp [r3 + r15]
vp_jmp [r5 + r13]
vp_jmp [r6 + r12]
vp_jmp [r7 + r11]
vp_jmp [r8 + r10]
vp_jmp [r9 + r9]
vp_jmp [r10 + r8]
vp_jmp [r11 + r7]
vp_jmp [r12 + r6]
vp_jmp [r13 + r5]
vp_jmp [r14 + r3]
vp_jmp [r15 + r2]
vp_jmp [r12 + r1]
vp_jmp [r1 + r13]
vp_jmp [r13 + r1]

vp_call [r0]
vp_call [r1]
vp_call [r2]
vp_call [r3]
vp_call [r4]
vp_call [r5]
vp_call [r6]
vp_call [r7]
vp_call [r8]
vp_call [r9]
vp_call [r10]
vp_call [r11]
vp_call [r12]
vp_call [r13]
vp_call [r14]
vp_call [r15]

vp_call [r0 + 0x7f]
vp_call [r1 + 0x7f]
vp_call [r2 + 0x7f]
vp_call [r3 + 0x7f]
vp_call [r4 + 0x7f]
vp_call [r5 + 0x7f]
vp_call [r6 + 0x7f]
vp_call [r7 + 0x7f]
vp_call [r8 + 0x7f]
vp_call [r9 + 0x7f]
vp_call [r10 + 0x7f]
vp_call [r11 + 0x7f]
vp_call [r12 + 0x7f]
vp_call [r13 + 0x7f]
vp_call [r14 + 0x7f]
vp_call [r15 + 0x7f]

vp_call [r0 + 0x80]
vp_call [r1 + 0x80]
vp_call [r2 + 0x80]
vp_call [r3 + 0x80]
vp_call [r4 + 0x80]
vp_call [r5 + 0x80]
vp_call [r6 + 0x80]
vp_call [r7 + 0x80]
vp_call [r8 + 0x80]
vp_call [r9 + 0x80]
vp_call [r10 + 0x80]
vp_call [r11 + 0x80]
vp_call [r12 + 0x80]
vp_call [r13 + 0x80]
vp_call [r14 + 0x80]
vp_call [r15 + 0x80]

vp_call [r0 + (label1 - label2)]
vp_call [r1 + (label1 - label2)]
vp_call [r2 + (label1 - label2)]
vp_call [r3 + (label1 - label2)]
vp_call [r4 + (label1 - label2)]
vp_call [r5 + (label1 - label2)]
vp_call [r6 + (label1 - label2)]
vp_call [r7 + (label1 - label2)]
vp_call [r8 + (label1 - label2)]
vp_call [r9 + (label1 - label2)]
vp_call [r10 + (label1 - label2)]
vp_call [r11 + (label1 - label2)]
vp_call [r12 + (label1 - label2)]
vp_call [r13 + (label1 - label2)]
vp_call [r14 + (label1 - label2)]
vp_call [r15 + (label1 - label2)]

vp_jmp [r0]
vp_jmp [r1]
vp_jmp [r2]
vp_jmp [r3]
vp_jmp [r4]
vp_jmp [r5]
vp_jmp [r6]
vp_jmp [r7]
vp_jmp [r8]
vp_jmp [r9]
vp_jmp [r10]
vp_jmp [r11]
vp_jmp [r12]
vp_jmp [r13]
vp_jmp [r14]
vp_jmp [r15]

vp_jmp [r0 + 0x7f]
vp_jmp [r1 + 0x7f]
vp_jmp [r2 + 0x7f]
vp_jmp [r3 + 0x7f]
vp_jmp [r4 + 0x7f]
vp_jmp [r5 + 0x7f]
vp_jmp [r6 + 0x7f]
vp_jmp [r7 + 0x7f]
vp_jmp [r8 + 0x7f]
vp_jmp [r9 + 0x7f]
vp_jmp [r10 + 0x7f]
vp_jmp [r11 + 0x7f]
vp_jmp [r12 + 0x7f]
vp_jmp [r13 + 0x7f]
vp_jmp [r14 + 0x7f]
vp_jmp [r15 + 0x7f]

vp_jmp [r0 + 0x80]
vp_jmp [r1 + 0x80]
vp_jmp [r2 + 0x80]
vp_jmp [r3 + 0x80]
vp_jmp [r4 + 0x80]
vp_jmp [r5 + 0x80]
vp_jmp [r6 + 0x80]
vp_jmp [r7 + 0x80]
vp_jmp [r8 + 0x80]
vp_jmp [r9 + 0x80]
vp_jmp [r10 + 0x80]
vp_jmp [r11 + 0x80]
vp_jmp [r12 + 0x80]
vp_jmp [r13 + 0x80]
vp_jmp [r14 + 0x80]
vp_jmp [r15 + 0x80]

vp_jmp [r0 + (label1 - label2)]
vp_jmp [r1 + (label1 - label2)]
vp_jmp [r2 + (label1 - label2)]
vp_jmp [r3 + (label1 - label2)]
vp_jmp [r4 + (label1 - label2)]
vp_jmp [r5 + (label1 - label2)]
vp_jmp [r6 + (label1 - label2)]
vp_jmp [r7 + (label1 - label2)]
vp_jmp [r8 + (label1 - label2)]
vp_jmp [r9 + (label1 - label2)]
vp_jmp [r10 + (label1 - label2)]
vp_jmp [r11 + (label1 - label2)]
vp_jmp [r12 + (label1 - label2)]
vp_jmp [r13 + (label1 - label2)]
vp_jmp [r14 + (label1 - label2)]
vp_jmp [r15 + (label1 - label2)]

;div
vp_div r1, r0, r2
vp_div r0, r0, r2
vp_div r8, r0, r2
vp_div r1, r2, r0
vp_div r7, r2, r0
vp_div r8, r2, r0
vp_div r15, r2, r0
vp_div r15, r3, r0
vp_div r0, r5, r9
vp_div r13, r2, r5
vp_div r4, r7, r8
vp_div_u r1, r0, r2
vp_div_u r7, r0, r2
vp_div_u r8, r0, r2
vp_div_u r1, r2, r0
vp_div_u r7, r2, r0
vp_div_u r8, r2, r0
vp_div_u r15, r2, r0
vp_div_u r15, r3, r0
vp_div_u r0, r5, r9
vp_div_u r13, r2, r5
vp_div_u r4, r7, r8

;ext
vp_ext r0, r1
vp_ext r0, r2
vp_ext r15, r3
vp_ext r10, r1
vp_ext r11, r13
vp_ext r5, r2
vp_ext r8, r7
vp_ext r4, r12

;branches
vp_beq label1
vp_bne label1
vp_blt label1
vp_ble label1
vp_bgt label1
vp_bge label1
vp_beq label2
vp_bne label2
vp_blt label2
vp_ble label2
vp_bgt label2
vp_bge label2

;syscall
syscall

	align 8, db 0
label1:
	dq 0xfe78452396457623

def_func_end
