%include 'inc/func.inc'
%include 'inc/heap.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/test4
		;task started by test1

		;sleep for 5 seconds
		vp_cpy 5000000, r0
		class_call task, sleep

		;init heap instance from stack
		;16 byte objects from 16 objects per block
		vp_sub hp_heap_size, r4
		vp_cpy r4, r0
		vp_cpy 16, r1
		vp_cpy 16*16, r2
		class_call heap, init

		;allocate 100 objects and print addresses
		for r8, 0, 100, 1
			vp_cpy r4, r0
			class_call heap, alloc
			vp_cpy r1, r0
			vp_cpy 10, r1
			vp_cpy 1, r2
			class_call io, number
			vp_cpy ' ', r0
			class_call io, char
		next
		vp_cpy 10, r0
		class_call io, char
		class_call io, char

		;free all objects
		vp_cpy r4, r0
		class_call heap, reset

		;deinit heap
		class_call heap, deinit
		vp_add hp_heap_size, r4

		;deshedule
		class_call task, yield

		for r10, 0, 10, 1
			;allocate 8MB - 8 general ram
			vp_cpy 0x800000 - 8, r0
			class_call mem, alloc
			fn_assert r0, !=, 0
			vp_cpy r0, r8
			vp_cpy r1, r9

			;print address and length
			vp_cpy 1, r1
			vp_cpy 10, r2
			class_call io, number
			vp_cpy ':', r0
			class_call io, char
			vp_cpy r9, r0
			vp_cpy 10, r2
			class_call io, number
			vp_cpy 10, r0
			class_call io, char

			;write to all locations
			vp_lea [r8 + r9], r12
			for r11, r8, r12, 8
				vp_cpy r8, [r11]
			next

			;free general ram
			vp_cpy r8, r0
			class_call mem, free
		next

		;print lf and return
		vp_cpy 1, r1
		vp_cpy 10, r0
		class_jmp io, char

	fn_function_end
