%include "func.inc"

;;;;;;;;;;;;;;;
; test function
;;;;;;;;;;;;;;;

	fn_function "tests/test1"
		;trashes
		;r0-r3, r5

		fn_call tests/test2
		kn_call KERNEL_STOP_TASK
	fn_function_end
