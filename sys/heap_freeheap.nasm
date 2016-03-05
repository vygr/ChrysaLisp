%include 'inc/func.inc'
%include 'inc/heap.inc'

	fn_function sys/heap_reset, no_debug_enter
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;trashes
		;r1-r3, r5

		vp_cpy 0, r1
		vp_cpy [r0 + hp_heap_blocklist], r2
		loop_start
			breakif r2, ==, 0
			vp_lea [r2 + hp_block_size], r3
			vp_cpy r3, r5
			vp_add [r0 + hp_heap_blocksize], r5
			loop_start
				vp_cpy r1, [r3 + hp_cell_next]
				vp_cpy r3, r1
				vp_add [r0 + hp_heap_cellsize], r3
			loop_until r3, >=, r5
			vp_cpy [r2 + hp_block_next], r2
		loop_end
		vp_cpy r1, [r0 + hp_heap_freelist]
		vp_ret

	fn_function_end
