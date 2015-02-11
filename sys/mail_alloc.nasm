%include "func.inc"

	fn_function "sys/mail_alloc"
		;outputs
		;r0 = mail message
		;trashes
		;r1-r3

		fn_call sys/mail_mailheap
		fn_call sys/heap_alloccell
		vp_cpy r1, r0
		vp_ret

	fn_function_end
