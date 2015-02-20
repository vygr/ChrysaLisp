%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_alloc"
		;outputs
		;r0 = mail message
		;trashes
		;r1-r3

		fn_bind sys/mail_statics, r0
		vp_lea [r0 + ML_STATICS_HEAP], r0
		fn_call sys/heap_alloccell
		vp_cpy ML_MSG_DATA, long[r1 + ML_MSG_LENGTH]
		vp_cpy r1, r0
		vp_ret

	fn_function_end
