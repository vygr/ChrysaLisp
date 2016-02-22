%include "func.inc"
%include "heap.inc"

	fn_function "sys/mem_alloc"
		;inputs
		;r0 = minimum amount in bytes
		;outputs
		;r0 = 0 if failed, else address
		;r1 = 0 if failed, else size given
		;trashes
		;r2-r3

		if r0, >, 0x800000 - 8
			;error
			vp_xor r0, r0
			vp_xor r1, r1
			vp_ret
		endif
		vp_lea [r0 + 8], r1		;extra 8 bytes for heap pointer
		vp_add 0x400-1, r1
		vp_and -0x400, r1	;at least 1KB bytes !

		;get statics
		fn_bind sys/mem_statics, r0

		;find object heap
		loop_while r1, >, [r0 + HP_HEAP_CELLSIZE]
			vp_add HP_HEAP_SIZE, r0
		loop_end

		;allocate object from this heap
		fn_call sys/heap_alloccell
		vp_cpy r0, [r1]
		vp_xchg r0, r1
		vp_cpy [r1 + HP_HEAP_CELLSIZE], r1
		vp_add 8, r0
		vp_sub 8, r1
		vp_ret

	fn_function_end
