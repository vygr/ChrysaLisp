%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_read"
		;inputs
		;r0 = mailbox address
		;outputs
		;r0 = mailbox address
		;r1 = mail address
		;trashes
		;r0-r2, r14

		lh_is_empty r0, r1
		if r1, ==, 0
			vp_cpy r15, [r0 + ML_MAILBOX_TCB]
			fn_call sys/task_suspend
		endif
		lh_remove_tail r0, r1, r2
		vp_ret

	fn_function_end
