%include "func.inc"
%include "mail.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	PIPE_SIZE equ 8

	fn_function "tests/test9"
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
			vp_cpy r1, [r0 + ML_MSG_DEST]
			vp_cpy r2, [r0 + (ML_MSG_DEST + 8)]
			fn_call sys/mail_send
			fn_call sys/task_deshedule
		next

		;free ID array
		vp_cpy r14, r0
		fn_call sys/mem_free

		;stop this task
		fn_jmp sys/task_stop

	task_tens:
		db 'tests/test10', 0
		db 'tests/test10', 0
		db 'tests/test10', 0
		db 'tests/test10', 0
		db 'tests/test10', 0
		db 'tests/test10', 0
		db 'tests/test10', 0
		db 'tests/test10', 0
		db 0

	fn_function_end
