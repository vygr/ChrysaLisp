%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_send"
		;inputs
		;r0 = mail message
		;trashes
		;r0-r2, r14

		vp_cpy [r0 + ML_MSG_DEST], r1
		if r1, ==, 0
			;mail for kernel !
			vp_cpy [rel kernel_mailbox], r1	;filled in by bootstrap
		endif
		lh_add_at_head r1, r0, r2
		vp_cpy [r1 + ML_MAILBOX_TCB], r0
		if r0, !=, 0
			vp_cpy 0, long[r1 + ML_MAILBOX_TCB]
			fn_call sys/task_resume
		endif
		vp_ret

		align 8, db 0
	kernel_mailbox:
		dq	-1

	fn_function_end
