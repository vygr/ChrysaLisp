%include "func.inc"
%include "heap.inc"

	fn_function "sys/mem_alloc"
		;inputs
		;r0 = minimum amount in bytes
		;outputs
		;r0 = 0 if failed, else address
		;r1 = 0 if failed, else size given
		;trashes
		;r2-r3, r5

		if r0, >, 0x800000 - 8
			;error
			vp_xor r0, r0
			vp_xor r1, r1
			vp_ret
		endif
		vp_add 8, r0		;extra 8 bytes for heap pointer
		vp_add 0x400-1, r0
		vp_and -0x400, r0	;at least 1KB bytes !

		;round to next power of 2
		vp_dec r0
		vp_cpy r0, r1
		vp_shr 1, r1
		vp_or r1, r0
		vp_cpy r0, r1
		vp_shr 2, r1
		vp_or r1, r0
		vp_cpy r0, r1
		vp_shr 4, r1
		vp_or r1, r0
		vp_cpy r0, r1
		vp_shr 8, r1
		vp_or r1, r0
		vp_cpy r0, r1
		vp_shr 16, r1
		vp_or r1, r0
		vp_cpy r0, r1
		vp_shr 32, r1
		vp_or r1, r0
		vp_lea [r0 + 1], r1

		;get statics
		fn_bind sys/mem_statics, r0

		;point to object heap
		vp_sub HP_HEAP_SIZE*10, r0
		vp_cpy r1, r5
		loop_start
			vp_add HP_HEAP_SIZE, r0
			vp_shr 1, r1
		loop_until r1, ==, 1

		;allocate object from this heap
		fn_call sys/heap_alloccell
		vp_cpy r0, [r1]
		vp_lea [r1 + 8], r0
		vp_lea [r5 - 8], r1
		vp_ret

	fn_function_end
