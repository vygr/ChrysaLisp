%include 'inc/func.inc'
%include 'inc/task.inc'

	def_function sys/mail_mymail
		;outputs
		;r0 = mail address
		;trashes
		;r1-r2

		s_call sys_task, mailbox, {}, {r0, r1}
		s_jmp sys_mail, read, {r0}, {r0}

	def_function_end
