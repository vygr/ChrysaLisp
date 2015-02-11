%include "func.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test1"
		;task started by test kernel

		;start test3 task
		vp_lea [rel task_two], r0
		kn_call KERNEL_LOAD_FUNCTION
		kn_call KERNEL_START_TASK

		;send test3 task 1000000 messages
		vp_cpy r0, r5
		vp_cpy 0, r6
		for r8, 0, 1000000, 1
			kn_call KERNEL_ALLOC_MAIL
			vp_cpy r5, [r0 + ML_MSG_DEST]
			vp_cpy r6, [r0 + (ML_MSG_DEST + 8)]
			kn_call KERNEL_SEND_MAIL
			kn_call KERNEL_DESHEDULE_TASK
		next

		;make test2 function call
		fn_call tests/test2

		;stop this task
		kn_call KERNEL_STOP_TASK

	task_two:
		db 'tests/test3', 0

	fn_function_end
