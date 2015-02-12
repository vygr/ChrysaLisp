%include "func.inc"
%include "heap.inc"

	fn_function "sys/mem_deinit_allocator"
		;get statics
		fn_call sys/mem_get_statics
		vp_cpy r0, r5

		;14 heaps, from 1KB bytes to 8MB
		for r6, 0, 14, 1
			vp_cpy r5, r0
			fn_call sys/heap_deinit
			vp_add HP_HEAP_SIZE, r5
		next
		vp_ret

	fn_function_end
