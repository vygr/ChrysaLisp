%include 'inc/func.ninc'
%include 'inc/task.ninc'

def_func sys/mail_try_read
	;inputs
	;r0 = mailbox address
	;outputs
	;r0 = 0, else mail address
	;trashes
	;r1-r2

	lh_get_head r0, r0
	vp_cpy r0, r1
	ln_get_succ r0, r0
	vpif r0, !=, 0
		vp_cpy r1, r0
		ln_remove_node r1, r2
	endif
	vp_ret

def_func_end
