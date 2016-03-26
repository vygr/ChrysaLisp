%include 'inc/func.inc'
%include 'inc/heap.inc'

	mem_slots equ 14

	fn_function sys/mem_init, no_debug_enter
		;get statics
		static_bind sys_mem, statics, r0

		;mem_slots heaps, from 1KB bytes to 8MB
		vp_cpy 0x400, r1				;start object size
		for r3, 0, mem_slots, 1
			vp_cpy mem_slots, r2
			vp_sub r3, r2				;from mem_slots to 1 objects per slot
			vp_mul r1, r2
			static_call sys_heap, init
			vp_add hp_heap_size, r0
			vp_add r1, r1				;double object size
		next
		vp_ret

	fn_function_end
