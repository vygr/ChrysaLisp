%include "func.inc"

;;;;;;;;;;;;;;;
; test function
;;;;;;;;;;;;;;;

	fn_function "tests/test1"
		;trashes
		;r0-r3, r5

		vp_cpy 1234567890, r0
		fn_call tests/test2
		vp_ret
	fn_function_end
