%include 'inc/func.inc'
%include 'inc/heap.inc'

	fn_function sys/heap_init
		;inputs
		;r0 = heap
		;r1 = cell size
		;r2 = block size
		;outputs
		;r0 = heap
		;r1 = cell size
		;r2 = block size

		vp_cpy 0, qword[r0 + hp_heap_freelist]
		vp_cpy 0, qword[r0 + hp_heap_blocklist]
		vp_add hp_cell_size - 1, r1
		vp_and -hp_cell_size, r1
		vp_cpy r1, [r0 + hp_heap_cellsize]
		vp_cpy r2, [r0 + hp_heap_blocksize]
		vp_ret

	fn_function_end
