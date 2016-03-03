%include 'inc/func.inc'
%include 'inc/heap.inc'

	fn_function sys/mem_deinit
		;get statics
		fn_bind sys/mem_statics, r0
		vp_cpy r0, r5

		;14 heaps, from 1KB bytes to 8MB
		for r6, 0, 14, 1
			vp_cpy r5, r0
			fn_call sys/heap_deinit
			vp_add HP_HEAP_SIZE, r5
		next
		vp_ret

	fn_function_end
