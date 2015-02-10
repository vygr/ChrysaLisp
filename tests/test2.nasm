%include "func.inc"

;;;;;;;;;;;;;;;
; test function
;;;;;;;;;;;;;;;

	fn_function "tests/test2"
		;inputs
		;r0 = number to print to stdout
		;trashes
		;r0-r3, r5

		vp_cpy 1, r1
		kn_call KERNEL_PRINT_NUM
		vp_ret
	fn_function_end
