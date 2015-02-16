%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_alloc"
		;outputs
		;r0 = mail message
		;trashes
		;r1-r3

		fn_call sys/mail_get_statics
		fn_call sys/heap_alloccell
		vp_cpy ML_MSG_DATA, long[r0 + ML_MSG_SIZE]
		vp_cpy r1, r0
		vp_ret

	fn_function_end
