%include "func.inc"
%include "heap.inc"

	fn_function "sys/heap_init"
		;inputs
		;r0 = heap
		;r1 = cell size
		;r2 = block size
		;outputs
		;r0 = heap
		;r1 = cell size
		;r2 = block size

		vp_cpy 0, qword[r0 + HP_HEAP_FREELIST]
		vp_cpy 0, qword[r0 + HP_HEAP_BLOCKLIST]
		vp_add HP_CELL_SIZE - 1, r1
		vp_and -HP_CELL_SIZE, r1
		vp_cpy r1, [r0 + HP_HEAP_CELLSIZE]
		vp_cpy r2, [r0 + HP_HEAP_BLOCKSIZE]
		vp_ret

	fn_function_end
