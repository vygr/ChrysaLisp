%include "func.inc"
%include "heap.inc"
%include "syscall.inc"

	fn_function "sys/heap_alloccell"
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;r1 = cell
		;trashes
		;r2-r3

		loopstart
			vp_cpy [r0 + HP_HEAP_FREELIST], r1
			if r1, !=, 0
				vp_cpy [r1 + HP_CELL_NEXT], r2
				vp_cpy r2, [r0 + HP_HEAP_FREELIST]
				vp_ret
			endif
			vp_cpy [r0 + HP_HEAP_BLOCKSIZE], r1
			vp_add HP_BLOCK_SIZE, r1
			vp_cpy r0, r2
			sys_mmap 0, r1, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0
			vp_cpy r0, r1
			vp_cpy r2, r0
			vp_cpy [r0 + HP_HEAP_BLOCKLIST], r2
			vp_cpy r2, [r1 + HP_BLOCK_NEXT]
			vp_cpy r1, [r0 + HP_HEAP_BLOCKLIST]
			vp_add HP_BLOCK_SIZE, r1
			vp_cpy r1, r3
			vp_add [r0 + HP_HEAP_BLOCKSIZE], r3
			vp_xor r2, r2
			loopstart
				vp_cpy r2, [r3 + HP_CELL_NEXT]
				vp_cpy r1, r2
				vp_add [r0 + HP_HEAP_CELLSIZE], r1
			until r1, >=, r3
			vp_cpy r2, [r0 + HP_HEAP_FREELIST]
		loopend

	fn_function_end
