%include "func.inc"

	fn_function "sys/mail_deinit_mailer"

		;deinit mail message heap
		fn_call sys/mail_mailheap
		fn_call sys/heap_deinit
		vp_ret

	fn_function_end
