%include 'inc/func.inc'
%include 'inc/heap.inc'

	fn_function sys/mem_init, no_debug_enter
		;get statics
		static_bind sys_mem, statics, r0

		;mem_slots heaps
		vp_cpy mem_block_min_size, r1	;start object size
		vp_cpy mem_block_max_size, r3	;start block size
		loop_start
			vp_cpy r3, r2
			static_call sys_heap, init
			vp_add hp_heap_size, r0
			vp_add r1, r1				;double object size
			vp_shr 1, r3				;half the block size
			if r3, <, r1
				vp_cpy r1, r3			;at least 1 object per block !
			endif
		loop_until r1, >, mem_block_max_size
		vp_ret

	fn_function_end
