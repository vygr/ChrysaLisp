%include "func.inc"
%include "heap.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test4"
		;task started by test1

		;init heap instance from stack
		;16 byte objects from 16 objects per block
		vp_sub HP_HEAP_SIZE, r4
		vp_cpy r4, r0
		vp_cpy 16, r1
		vp_cpy 16*16, r2
		fn_call sys/heap_init

		;allocate 100 objects and print addresses
		for r8, 0, 100, 1
			vp_cpy r4, r0
			fn_call sys/heap_alloccell
			vp_cpy r1, r0
			vp_cpy 1, r1
			fn_call sys/write_number
			vp_cpy ' ', r0
			fn_call sys/write_char
		next
		vp_cpy 10, r0
		fn_call sys/write_char
		fn_call sys/write_char

		;free all objects
		vp_cpy r4, r0
		fn_call sys/heap_freeheap

		;deinit heap
		fn_call sys/heap_deinit
		vp_add HP_HEAP_SIZE, r4

		;stop this task
		fn_jmp sys/task_stop

	fn_function_end
