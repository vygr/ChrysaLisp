%include 'inc/func.inc'
%include 'inc/heap.inc'

	fn_function sys/heap_init, no_debug_enter
		;inputs
		;r0 = heap
		;r1 = cell size
		;r2 = block size
		;outputs
		;r0 = heap
		;r1 = cell size
		;r2 = block size

		vp_cpy_cl 0, [r0 + hp_heap_freelist]
		vp_cpy_cl 0, [r0 + hp_heap_blocklist]
		vp_add 7, r1
		vp_and -8, r1
		vp_cpy r1, [r0 + hp_heap_cellsize]
		vp_cpy r2, [r0 + hp_heap_blocksize]
		vp_ret

	fn_function_end
