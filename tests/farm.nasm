%include 'inc/func.inc'
%include 'inc/mail.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	FARM_SIZE equ 128

	fn_function tests/farm

		;allocate temp array for mailbox ID's
		static_call sys_mem, alloc, {mailbox_id_size * FARM_SIZE}, {r14, _}
		assert r0, !=, 0

		;open farm, off chip
		static_call sys_task, open_farm, {"tests/farm_child", r14, FARM_SIZE}

		;send exit messages etc
		for r13, 0, FARM_SIZE, 1
			static_call sys_mail, alloc, {}, {r0}
			assert r0, !=, 0
			vp_cpy r13, r3
			vp_mul mailbox_id_size, r3
			vp_cpy [r14 + r3], r1
			vp_cpy [r14 + r3 + 8], r2
			vp_cpy r1, [r0 + ml_msg_dest]
			vp_cpy r2, [r0 + (ml_msg_dest + 8)]
			static_call sys_mail, send, {r0}
		next

		;free ID array and return
		static_jmp sys_mem, free, {r14}

	fn_function_end
