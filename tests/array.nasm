%include 'inc/func.inc'
%include 'inc/mail.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	ARRAY_SIZE equ 128

	fn_function tests/array

		;allocate temp array for mailbox ID's
		vp_cpy mailbox_id_size * ARRAY_SIZE, r0
		static_call sys_mem, alloc
		fn_assert r0, !=, 0
		vp_cpy r0, r14

		;open test8 array, off chip
		vp_cpy r14, r1
		vp_lea [rel task_eights], r0
		static_call sys_task, open_array

		;send exit messages etc
		for r13, 0, ARRAY_SIZE, 1
			static_call sys_mail, alloc
			fn_assert r0, !=, 0
			vp_cpy r13, r3
			vp_mul mailbox_id_size, r3
			vp_cpy [r14 + r3], r1
			vp_cpy [r14 + r3 + 8], r2
			vp_cpy r1, [r0 + ml_msg_dest]
			vp_cpy r2, [r0 + (ml_msg_dest + 8)]
			static_call sys_mail, send
			static_call sys_task, yield
		next

		;free ID array and return
		vp_cpy r14, r0
		static_jmp sys_mem, free

	task_eights:
		%rep ARRAY_SIZE
		db 'tests/array_child', 0
		%endrep
		db 0

	fn_function_end
