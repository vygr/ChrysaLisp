%include 'inc/func.inc'
%include 'inc/heap.inc'

	def_function sys/heap_reset
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;trashes
		;r1-r3, r5

		vp_xor r1, r1
		vp_cpy [r0 + hp_heap_block_flist], r2
		loop_start
			breakif r2, ==, 0
			vp_lea [r2 + ln_fnode_size], r3
			vp_cpy r3, r5
			vp_add [r0 + hp_heap_blocksize], r5
			loop_start
				vp_cpy r1, [r3 + ln_fnode_next]
				vp_cpy r3, r1
				vp_add [r0 + ln_fnode_size], r3
			loop_until r3, >=, r5
			vp_cpy [r2 + ln_fnode_next], r2
		loop_end
		vp_cpy r1, [r0 + hp_heap_free_flist]
		vp_ret

	def_function_end
