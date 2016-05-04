%include 'inc/func.inc'
%include 'inc/mail.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	PIPE_SIZE equ 128

	fn_function tests/pipe

		;allocate temp array for mailbox ID's
		s_call sys_mem, alloc, {mailbox_id_size * PIPE_SIZE}, {r14, _}
		assert r0, !=, 0

		;open pipe, off chip
		s_call sys_task, open_pipe, {$child_tasks, r14}

		;send exit messages etc
		for r13, 0, PIPE_SIZE, 1
			s_call sys_mail, alloc, {}, {r0}
			assert r0, !=, 0
			vp_cpy r13, r3
			vp_mul mailbox_id_size, r3
			vp_cpy [r14 + r3], r1
			vp_cpy [r14 + r3 + 8], r2
			vp_cpy r1, [r0 + ml_msg_dest]
			vp_cpy r2, [r0 + (ml_msg_dest + 8)]
			s_call sys_mail, send, {r0}
			s_call sys_task, yield
		next

		;free ID array and return
		s_jmp sys_mem, free, {r14}

	child_tasks:
		%rep PIPE_SIZE
			db 'tests/pipe_child', 0
		%endrep
		db 0

	fn_function_end
