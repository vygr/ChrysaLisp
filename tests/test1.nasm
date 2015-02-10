%include "func.inc"

;;;;;;;;;;;;;;;
; test function
;;;;;;;;;;;;;;;

	fn_function "tests/test1"
		;task started by test kernel

		vp_lea [rel task_two], r0
		kn_call KERNEL_LOAD_FUNCTION
		kn_call KERNEL_START_TASK

		fn_call tests/test2
		kn_call KERNEL_STOP_TASK

	task_two:
		db 'tests/test3', 0

	fn_function_end
