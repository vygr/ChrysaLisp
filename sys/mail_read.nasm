%include "inc/func.inc"
%include "inc/task.inc"

	fn_function "sys/mail_read"
		;inputs
		;r0 = mailbox address
		;outputs
		;r0 = mail address
		;trashes
		;r1-r2

		lh_is_empty r0, r2
		if r2, ==, 0
			fn_bind sys/task_statics, r1
			vp_cpy [r1 + TK_STATICS_CURRENT_TCB], r1
			vp_cpy r1, [r0 + ML_MAILBOX_TCB]
			fn_call sys/task_suspend
		endif
		vp_cpy [r0 + ML_MAILBOX_LIST + LH_LIST_HEAD], r0
		vp_cpy r0, r1
		ln_remove_node r1, r2
		vp_ret

	fn_function_end
