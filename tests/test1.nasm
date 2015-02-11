%include "func.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test1"
		;task started by test kernel

		;start test4 task
		vp_lea [rel task_four], r0
		kn_call KERNEL_FUNCTION_LOAD
		kn_call KERNEL_TASK_START

		;start test3 task
		vp_lea [rel task_three], r0
		kn_call KERNEL_FUNCTION_LOAD
		kn_call KERNEL_TASK_START

		;send test3 task 1000000 messages
		vp_cpy r0, r5
		vp_cpy 0, r6
		for r8, 0, 1000000, 1
			fn_call sys/mail_alloc
			vp_cpy r5, [r0 + ML_MSG_DEST]
			vp_cpy r6, [r0 + (ML_MSG_DEST + 8)]
			fn_call sys/mail_send
			kn_call KERNEL_TASK_DESHEDULE
		next

		;make test2 function call
		fn_call tests/test2

		;stop this task
		kn_call KERNEL_TASK_STOP

	task_three:
		db 'tests/test3', 0
	task_four:
		db 'tests/test4', 0

	fn_function_end
