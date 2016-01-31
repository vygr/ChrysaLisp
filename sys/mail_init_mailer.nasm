%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_init_mailer"

		;init off chip list
		fn_bind sys/mail_statics, r0
		vp_lea [r0 + ML_STATICS_OFFCHIP_LIST], r1
		lh_init r1, r2

		;init mail message heap
		vp_lea [r0 + ML_STATICS_HEAP], r0
		vp_cpy ML_MSG_SIZE, r1
		vp_cpy ML_MSG_SIZE*256, r2
		fn_jmp sys/heap_init

	fn_function_end
