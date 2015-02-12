%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_init_mailer"

		;init mail message heap
		fn_call sys/mail_get_statics
		vp_cpy ML_MSG_SIZE, r1
		vp_cpy ML_MSG_SIZE*256, r2
		fn_jmp sys/heap_init

	fn_function_end
