%include 'inc/func.ninc'
%include 'inc/task.ninc'

def_func sys/mail_mymail
	;outputs
	;r0 = mail address
	;trashes
	;r1-r2

	f_call sys_task, mailbox, {}, {r0, r1}
	f_jmp sys_mail, read, {r0}

def_func_end
