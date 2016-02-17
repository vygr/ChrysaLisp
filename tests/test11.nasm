%include "func.inc"
%include "task.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test11"
		;task started by test1

		;get max cpu num
		fn_call sys/get_cpu_total
		vp_cpy r0, r12

		;allocate temp array for mailbox ID's
		vp_mul 16, r0
		fn_call sys/mem_alloc
		vp_cpy r0, r14

		;open test12 pipe, off chip
		vp_lea [rel task_twelve], r0
		vp_cpy r14, r1
		vp_cpy r12, r2
		fn_call sys/task_open_global

		;send messages etc
		for r11, 0, 10, 1
			for r13, 0, r12, 1
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
		next

		;free ID array
		vp_cpy r14, r0
		fn_call sys/mem_free

		;stop this task
		fn_jmp sys/task_stop

	task_twelve:
		db 'tests/test12', 0
		db 0

	fn_function_end
