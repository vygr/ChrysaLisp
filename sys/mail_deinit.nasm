%include 'inc/func.inc'

def_func sys/mail_deinit

	;deinit mail message heap
	f_bind sys_mail, statics, r0
	f_jmp sys_heap, deinit, {[r0 + ml_statics_heap]}

def_func_end
