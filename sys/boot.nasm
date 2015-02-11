%include "func.inc"

;;;;;;;;;;;
; boot task
;;;;;;;;;;;

	fn_function "sys/boot"
		;task started by kernel on boot

		;start test1 task
		vp_lea [rel task_one], r0
		kn_call KERNEL_LOAD_FUNCTION
		kn_call KERNEL_START_TASK

		;stop this task
		kn_call KERNEL_STOP_TASK

	task_one:
		db 'tests/test1', 0

	fn_function_end
