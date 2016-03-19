%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/mail_mymail, no_debug_enter
		;outputs
		;r0 = mail address
		;trashes
		;r1-r2

		static_call task, mailbox
		static_jmp mail, read

	fn_function_end
