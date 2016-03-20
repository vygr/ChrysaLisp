%include 'inc/func.inc'
%include 'inc/task.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	TEST_SIZE equ 1000

	fn_function tests/global

		;get max cpu num
		static_call cpu, total
		vp_cpy r0, r12

		;allocate temp array for mailbox ID's
		vp_mul mailbox_id_size, r0
		static_call mem, alloc
		fn_assert r0, !=, 0
		vp_cpy r0, r14

		;open test12 global farm, off chip
		vp_lea [rel task_twelve], r0
		vp_cpy r14, r1
		vp_cpy r12, r2
		static_call task, global

		;send messages etc
		for r11, 0, 10, 1
			for r13, 0, r12, 1
				vp_cpy (TEST_SIZE * 8), r0
				static_call mail, parcel
				for r15, 0, TEST_SIZE, 1
					vp_cpy r15, [r0 + (r15 * 8) + ml_msg_data]
				next
				vp_cpy r13, r3
				vp_mul mailbox_id_size, r3
				vp_cpy [r14 + r3], r1
				vp_cpy [r14 + r3 + 8], r2
				vp_cpy r1, [r0 + ml_msg_dest]
				vp_cpy r2, [r0 + (ml_msg_dest + 8)]
				static_call mail, send
				static_call task, yield
			next
		next

		;free ID array and return
		vp_cpy r14, r0
		static_jmp mem, free

	task_twelve:
		db 'tests/global_child', 0

	fn_function_end
