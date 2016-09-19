%include 'inc/func.inc'
%include 'inc/heap.inc'
%include 'inc/syscall.inc'

	fn_function sys/heap_alloc
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;r1 = cell
		;trashes
		;r2-r3

		loop_start
			vp_cpy [r0 + hp_heap_free_flist], r1
			if r1, !=, 0
				vp_cpy [r1 + ln_fnode_next], r2
				vp_cpy r2, [r0 + hp_heap_free_flist]
				vp_ret
			endif
			vp_cpy [r0 + hp_heap_blocksize], r1
			vp_add ln_fnode_size, r1
			vp_cpy r0, r2
			sys_mmap 0, r1, prot_read|prot_write, map_private|map_anon, -1, 0
			assert r0, !=, 0
			s_bind sys_mem, statics, r3
			vp_add r1, [r3]
			vp_cpy r0, r1
			vp_cpy r2, r0
			ln_add_fnode r0 + hp_heap_block_flist, r1, r2
			vp_add ln_fnode_size, r1
			vp_cpy r1, r3
			vp_add [r0 + hp_heap_blocksize], r3
			vp_xor r2, r2
			loop_start
				vp_cpy r2, [r3 + ln_fnode_next]
				vp_cpy r1, r2
				vp_add [r0 + hp_heap_cellsize], r1
			loop_until r1, >=, r3
			vp_cpy r2, [r0 + hp_heap_free_flist]
		loop_end

	fn_function_end
