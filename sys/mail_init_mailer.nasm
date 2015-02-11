%include "func.inc"

	fn_function "sys/mail_init_mailer"

		;init mail message heap
		fn_call sys/mail_mailheap
		vp_cpy ML_MSG_SIZE, r1
		vp_cpy ML_MSG_SIZE*256, r2
		fn_call sys/heap_init
		vp_ret

	fn_function_end
