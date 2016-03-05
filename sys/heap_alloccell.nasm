%include 'inc/func.inc'
%include 'inc/heap.inc'
%include 'inc/syscall.inc'

	fn_function sys/heap_alloccell, no_debug_enter
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;r1 = cell
		;trashes
		;r2-r3

		loop_start
			vp_cpy [r0 + hp_heap_freelist], r1
			if r1, !=, 0
				vp_cpy [r1 + hp_cell_next], r2
				vp_cpy r2, [r0 + hp_heap_freelist]
				vp_ret
			endif
			vp_cpy [r0 + hp_heap_blocksize], r1
			vp_add hp_block_size, r1
			vp_cpy r0, r2
			sys_mmap 0, r1, prot_read|prot_write, map_private|map_anon, -1, 0
			fn_assert r0, !=, 0
			vp_cpy r0, r1
			vp_cpy r2, r0
			vp_cpy [r0 + hp_heap_blocklist], r2
			vp_cpy r2, [r1 + hp_block_next]
			vp_cpy r1, [r0 + hp_heap_blocklist]
			vp_add hp_block_size, r1
			vp_cpy r1, r3
			vp_add [r0 + hp_heap_blocksize], r3
			vp_xor r2, r2
			loop_start
				vp_cpy r2, [r3 + hp_cell_next]
				vp_cpy r1, r2
				vp_add [r0 + hp_heap_cellsize], r1
			loop_until r1, >=, r3
			vp_cpy r2, [r0 + hp_heap_freelist]
		loop_end

	fn_function_end
