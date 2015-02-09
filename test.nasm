%include "func.inc"

;;;;;;;;;;;;;;;
; test function
;;;;;;;;;;;;;;;

	fn_function "test"
		kn_call KERNEL_ALLOC_MAIL
		vp_cpy r0,r1
		kn_call KERNEL_FREE_MAIL
		vp_ret
	fn_function_end
