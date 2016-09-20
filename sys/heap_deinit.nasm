%include 'inc/func.inc'
%include 'inc/heap.inc'
%include 'inc/syscall.inc'

	def_function sys/heap_deinit
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;trashes
		;r0-r3

		vp_cpy r0, r1
		loop_flist_forward r0 + hp_heap_block_flist, r2, r3
			vp_cpy r2, r0
			ln_remove_fnode r2, r3
			vp_cpy [r1 + hp_heap_blocksize], r2
			vp_add ln_fnode_size, r2
			sys_munmap r0, r2
			s_bind sys_mem, statics, r0
			vp_sub r2, [r0]
		loop_end
		vp_ret

	def_function_end
