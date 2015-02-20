%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_send"
		;inputs
		;r0 = mail message
		;trashes
		;r0-r2

		;going off chip ?
		vp_cpy r0, r2
		fn_call sys/get_cpu_id
		if r0, ==, [r2 + (ML_MSG_DEST + 8)]
			;no on this chip
			vp_cpy [r2 + ML_MSG_DEST], r1
			if r1, ==, 0
				;mail for kernel !
				vp_cpy [rel kernel_mailbox], r1
			endif
			lh_add_at_head r1, r2, r0
			vp_cpy [r1 + ML_MAILBOX_TCB], r0
			if r0, !=, 0
				vp_cpy 0, long[r1 + ML_MAILBOX_TCB]
				fn_call sys/task_resume
			endif
		else
			;yes off chip
			fn_bind sys/mail_statics, r1
			vp_lea [r1 + ML_STATICS_OFFCHIP_LIST], r1
			lh_add_at_head r1, r2, r0
		endif
		vp_ret

		align 8, db 0
	kernel_mailbox:
		dq	-1	;filled in by bootstrap

	fn_function_end
