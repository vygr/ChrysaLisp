%include 'inc/func.inc'
%include 'inc/mail.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	PIPE_SIZE equ 16

	fn_function tests/test9
		;task started by test1

		;allocate temp array for mailbox ID's
		vp_cpy 16*PIPE_SIZE, r0
		fn_call sys/mem_alloc
		vp_cpy r0, r14

		;open test10 pipe, off chip
		vp_cpy r14, r1
		vp_lea [rel task_tens], r0
		fn_call sys/task_open_pipe

		;send exit messages etc
		for r13, 0, PIPE_SIZE, 1
			fn_call sys/mail_alloc
			vp_cpy r13, r3
			vp_mul 16, r3
			vp_cpy [r14 + r3], r1
			vp_cpy [r14 + r3 + 8], r2
			vp_cpy r1, [r0 + ml_msg_dest]
			vp_cpy r2, [r0 + (ml_msg_dest + 8)]
			fn_call sys/mail_send
			fn_call sys/task_yield
		next

		;free ID array and return
		vp_cpy r14, r0
		fn_jmp sys/mem_free

	task_tens:
		%rep PIPE_SIZE
		db 'tests/test10', 0
		%endrep
		db 0

	fn_function_end
