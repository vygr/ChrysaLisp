%include "func.inc"
%include "heap.inc"
%include "syscall.inc"

	fn_function "sys/heap_deinit"
		;inputs
		;r0 = heap
		;outputs
		;r0 = heap
		;trashes
		;r0-r3

		vp_cpy r0, r2
		vp_cpy [r2 + HP_HEAP_BLOCKLIST], r1
		loop_start
			breakif r1, ==, 0
			vp_cpy [r1 + HP_BLOCK_NEXT], r3
			sys_munmap r1, [r2 + HP_HEAP_BLOCKSIZE]
			vp_cpy r3, r1
		loop_end
		vp_ret

	fn_function_end
