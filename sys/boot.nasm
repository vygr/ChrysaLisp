%include "func.inc"

;;;;;;;;;;;
; boot task
;;;;;;;;;;;

	fn_function "sys/boot"
		;task started by kernel on boot

		;start test1 task
		vp_lea [rel task_one], r0
		fn_call sys/load_function_load
		fn_call sys/task_start

		;stop this task
		fn_jmp sys/task_stop

	task_one:
		db 'tests/test1', 0

	fn_function_end
