%include "func.inc"
%include "heap.inc"

	fn_function "sys/mem_init_allocator"
		;get statics
		fn_call sys/mem_get_statics
		vp_cpy r0,r5

		;14 heaps, from 1KB bytes to 8MB
		vp_cpy 0x400, r1				;start object size
		for r3, 0, 14, 1
			vp_xor r2, r2
			vp_cpy 0x800000, r0			;max size of block
			vp_div r1					;max objects for this block
			vp_mul r1, r0
			vp_cpy r0, r2				;size for this block 
			vp_cpy r5, r0
			fn_call sys/heap_init
			vp_add HP_HEAP_SIZE, r5
			vp_add r1, r1				;double object size
		next
		vp_ret

	fn_function_end
