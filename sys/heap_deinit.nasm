%include 'inc/func.inc'
%include 'inc/heap.inc'
%include 'inc/syscall.inc'

	fn_function sys/heap_deinit, no_debug_enter
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;trashes
		;r0-r3

		vp_cpy r0, r2
		vp_cpy [r2 + hp_heap_blocklist], r1
		loop_start
			breakif r1, ==, 0
			vp_cpy [r1 + hp_block_next], r3
			sys_munmap r1, [r2 + hp_heap_blocksize]
			vp_cpy r3, r1
		loop_end
		vp_ret

	fn_function_end
