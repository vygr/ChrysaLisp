%include "func.inc"
%include "task.inc"

	fn_function "sys/mail_read_mymail"
		;inputs
		;r15 = task control node
		;outputs
		;r0 = mailbox address
		;r1 = mail address
		;trashes
		;r0-r2

		vp_lea [r15 + TK_NODE_MAILBOX], r0
		fn_call sys/mail_read
		vp_ret

	fn_function_end
