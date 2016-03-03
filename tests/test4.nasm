%include 'inc/func.inc'
%include 'inc/heap.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/test4
		;task started by test1

		;sleep for 5 seconds
		vp_cpy 5000000, r0
		fn_call sys/task_sleep

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

		;deshedule
		fn_call sys/task_yield

		for r10, 0, 10, 1
			;allocate 8MB - 8 general ram
			vp_cpy 0x800000 - 8, r0
			fn_call sys/mem_alloc
			vp_cpy r0, r8
			vp_cpy r1, r9

			;print address and length
			vp_cpy 1, r1
			fn_call sys/write_number
			vp_cpy ':', r0
			fn_call sys/write_char
			vp_cpy r9, r0
			fn_call sys/write_number
			vp_cpy 10, r0
			fn_call sys/write_char

			;write to all locations
			vp_lea [r8 + r9], r12
			for r11, r8, r12, 8
				vp_cpy r8, [r11]
			next

			;free general ram
			vp_cpy r8, r0
			fn_call sys/mem_free
		next

		;print lf and return
		vp_cpy 1, r1
		vp_cpy 10, r0
		fn_jmp sys/write_char

	fn_function_end
