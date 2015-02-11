%include "func.inc"
%include "heap.inc"

	fn_function "sys/heap_freeheap"
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;trashes
		;r1-r3, r5

		vp_cpy 0, r1
		vp_cpy [r0 + HP_HEAP_BLOCKLIST], r2
		loopstart
			breakif r2, ==, 0
			vp_lea [r2 + HP_BLOCK_SIZE], r3
			vp_cpy r3, r5
			vp_add [r0 + HP_HEAP_BLOCKSIZE], r5
			repeat
				vp_cpy r1, [r3 + HP_CELL_NEXT]
				vp_cpy r3, r1
				vp_add [r0 + HP_HEAP_CELLSIZE], r3
			until r3, >=, r5
			vp_cpy [r2 + HP_BLOCK_NEXT], r2
		loopend
		vp_cpy r1, [r0 + HP_HEAP_FREELIST]
		vp_ret

	fn_function_end
