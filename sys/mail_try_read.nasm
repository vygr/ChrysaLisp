%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/mail_try_read, no_debug_enter
		;inputs
		;r0 = mailbox address
		;outputs
		;r0 = 0, else mail address
		;trashes
		;r1-r2

		lh_is_empty r0, r2
		if r2, ==, 0
			vp_cpy r2, r0
			vp_ret
		endif
		vp_cpy [r0 + ml_mailbox_list + lh_list_head], r0
		vp_cpy r0, r1
		ln_remove_node r1, r2
		vp_ret

	fn_function_end
