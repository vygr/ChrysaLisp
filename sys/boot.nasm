%include "func.inc"

;;;;;;;;;;;
; boot task
;;;;;;;;;;;

	fn_function "sys/boot"
		;task started by kernel on boot

		;start test1 task
		vp_lea [rel task_one], r0
		kn_call KERNEL_FUNCTION_LOAD
		kn_call KERNEL_TASK_START

		;stop this task
		kn_jmp KERNEL_TASK_STOP

	task_one:
		db 'tests/test1', 0

	fn_function_end
