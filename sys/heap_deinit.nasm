%include 'inc/func.inc'
%include 'inc/heap.inc'
%include 'inc/syscall.inc'

	fn_function sys/heap_deinit
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;trashes
		;r0-r3

		vp_cpy r0, r1
		loop_flist_forward r0 + hp_heap_blocklist, r2, r3
			vp_cpy r2, r0
			ln_remove_fnode r2, r3
			sys_munmap r0, [r1 + hp_heap_blocksize]
		loop_end
		vp_ret

	fn_function_end
