%include "func.inc"

;;;;;;;;;;;;;;;
; test function
;;;;;;;;;;;;;;;

	fn_function "test"
		kn_call KERNEL_ALLOC_MAIL
		vp_cpy r0,r1
		kn_call KERNEL_FREE_MAIL
		fn_call "gui/open_window"
		vp_cpy 0xff00ff00, r0
		fn_call "gui/clear_window"
		vp_ret
	fn_function_end
