%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/mail_read, no_debug_enter
		;inputs
		;r0 = mailbox address
		;outputs
		;r0 = mail address
		;trashes
		;r1-r2

		lh_is_empty r0, r2
		if r2, ==, 0
			static_bind sys_task, statics, r1
			vp_cpy [r1 + tk_statics_current_tcb], r1
			vp_cpy r1, [r0 + ml_mailbox_tcb]
			s_call sys_task, suspend
		endif
		lh_get_head r0, r0
		vp_cpy r0, r1
		ln_remove_node r1, r2
		vp_ret

	fn_function_end
