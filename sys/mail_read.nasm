%include 'inc/func.inc'
%include 'inc/task.inc'

def_func sys/mail_read
	;inputs
	;r0 = mailbox address
	;outputs
	;r0 = mail address
	;trashes
	;r1-r2

	lh_is_empty r0, r2
	if r2, ==, 0
		f_bind sys_task, statics, r1
		vp_cpy [r1 + tk_statics_current_tcb], r1
		vp_cpy r1, [r0 + mailbox_tcb]
		f_call sys_task, suspend
	endif
	lh_get_head r0, r0
	vp_cpy r0, r1
	ln_remove_node r1, r2
	vp_ret

def_func_end
