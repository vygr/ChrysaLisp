%include "func.inc"
%include "heap.inc"

	MEM_SLOTS equ 14

	fn_function "sys/mem_init_allocator"
		;get statics
		fn_bind sys/mem_statics, r0

		;MEM_SLOTS heaps, from 1KB bytes to 8MB
		vp_cpy 0x400, r1				;start object size
		for r3, 0, MEM_SLOTS, 1
			vp_cpy MEM_SLOTS, r2
			vp_sub r3, r2				;from MEM_SLOTS to 1 objects per slot
			vp_mul r1, r2
			fn_call sys/heap_init
			vp_add HP_HEAP_SIZE, r0
			vp_add r1, r1				;double object size
		next
		vp_ret

	fn_function_end
