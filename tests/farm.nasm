%include 'inc/func.inc'
%include 'inc/mail.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	FARM_SIZE equ 128

	fn_function tests/farm

		;allocate temp array for mailbox ID's
		vp_cpy mailbox_id_size * FARM_SIZE, r0
		static_call mem, alloc
		fn_assert r0, !=, 0
		vp_cpy r0, r14

		;open test6 farm, off chip
		vp_cpy FARM_SIZE, r2
		vp_cpy r14, r1
		vp_lea [rel task_six], r0
		static_call task, farm

		;send exit messages etc
		for r13, 0, FARM_SIZE, 1
			static_call mail, alloc
			fn_assert r0, !=, 0
			vp_cpy r13, r3
			vp_mul mailbox_id_size, r3
			vp_cpy [r14 + r3], r1
			vp_cpy [r14 + r3 + 8], r2
			vp_cpy r1, [r0 + ml_msg_dest]
			vp_cpy r2, [r0 + (ml_msg_dest + 8)]
			static_call mail, send
		next

		;free ID array and return
		vp_cpy r14, r0
		static_jmp mem, free

	task_six:
		db 'tests/farm_child', 0

	fn_function_end
