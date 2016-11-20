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

def_func_end
