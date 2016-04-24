%include 'inc/func.inc'
%include 'inc/task.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	TEST_SIZE equ 1000

	fn_function tests/global

		;get max cpu num
		static_call sys_cpu, total, '', 'r12'

		;allocate temp array for mailbox ID's
		vp_mul mailbox_id_size, r0
		static_call sys_mem, alloc, '', 'r14, r1'
		assert r0, !=, 0

		;open global farm, off chip
		static_call sys_task, open_global, '"tests/global_child", r14, r12'

		;send messages etc
		for r11, 0, 10, 1
			for r13, 0, r12, 1
				static_call sys_mail, alloc_parcel, 'TEST_SIZE * 8'
				for r15, 0, TEST_SIZE, 1
					vp_cpy r15, [r0 + (r15 * 8) + ml_msg_data]
				next
				vp_cpy r13, r3
				vp_mul mailbox_id_size, r3
				vp_cpy [r14 + r3], r1
				vp_cpy [r14 + r3 + 8], r2
				vp_cpy r1, [r0 + ml_msg_dest]
				vp_cpy r2, [r0 + ml_msg_dest + 8]
				static_call sys_mail, send
				static_call sys_task, yield
			next
		next

		;free ID array and return
		static_jmp sys_mem, free, 'r14'

	fn_function_end
