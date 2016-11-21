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

def_func_end
