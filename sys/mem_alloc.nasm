%include 'inc/func.inc'
%include 'inc/heap.inc'

	fn_function sys/mem_alloc
		;inputs
		;r0 = minimum amount in bytes
		;outputs
		;r0 = 0 if failed, else address
		;r1 = 0 if failed, else size given
		;trashes
		;r2-r3

		if r0, >, mem_block_max_size - ptr_size
			;error
			vp_xor r0, r0
			vp_xor r1, r1
			vp_ret
		endif
		vp_lea [r0 + ptr_size], r1		;extra 8 bytes for heap pointer

		;find object heap
		s_bind sys_mem, statics, r0
		vp_add long_size, r0
		loop_while r1, >, [r0 + hp_heap_cellsize]
			vp_add hp_heap_size, r0
		loop_end

		;allocate object from this heap
		s_call sys_heap, alloc, {r0}, {r1}
		vp_cpy r0, [r1]
		vp_xchg r0, r1
		vp_cpy [r1 + hp_heap_cellsize], r1
		vp_add ptr_size, r0
		vp_sub ptr_size, r1
		vp_ret

	fn_function_end
