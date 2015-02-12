%include "func.inc"
%include "heap.inc"

	fn_function "sys/mem_init_allocator"
		;get statics
		fn_call sys/mem_get_statics

		;14 heaps, from 1KB bytes to 8MB
		vp_cpy 0x400, r1				;start object size
		for r3, 0, 14, 1
			vp_cpy 0x800000, r2
			fn_call sys/heap_init
			vp_add HP_HEAP_SIZE, r0
			vp_add r1, r1				;double object size
		next
		vp_ret

	fn_function_end
