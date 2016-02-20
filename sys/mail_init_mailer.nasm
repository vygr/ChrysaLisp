%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_init_mailer"
		;inputs
		;r0 = kernel mailbox

		;save kernel mailbox
		fn_bind sys/mail_statics, r1
		vp_cpy r0, [r1 + ML_STATICS_KERNEL_MAILBOX]

		;init off chip list
		vp_lea [r1 + ML_STATICS_OFFCHIP_LIST], r0
		lh_init r0, r2

		;init mail message heap
		vp_lea [r1 + ML_STATICS_HEAP], r0
		vp_cpy ML_MSG_SIZE + 8, r1
		vp_cpy (ML_MSG_SIZE + 8) * 256, r2
		fn_jmp sys/heap_init

	fn_function_end
