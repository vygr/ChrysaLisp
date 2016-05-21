%include 'inc/func.inc'

	fn_function sys/mail_mailbox
		;outputs
		;r0 = mailbox address
		;trashes
		;r1-r2

		ml_init r0, r1, r2
		vp_ret

	fn_function_end
