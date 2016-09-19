%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_send
		;inputs
		;r0 = mail message
		;trashes
		;r0-r2

		;on or off chip ?
		vp_cpy r0, r2
		s_call sys_cpu, id, {}, {r0}
		if r0, ==, [r2 + (msg_dest + 8)]
			;on this chip
			vp_cpy [r2 + msg_parcel_size], r1
			if r1, !=, 0
				;mail for postman !
				s_bind sys_mail, statics, r1
				vp_cpy [r1 + ml_statics_in_mailbox], r1
			else
				vp_cpy [r2 + msg_dest], r1
				if r1, ==, 0
					;mail for kernel !
					s_bind sys_mail, statics, r1
					vp_cpy [r1 + ml_statics_kernel_mailbox], r1
				endif
			endif
		post_it:
			lh_add_at_tail r1, r2, r0
			vp_cpy [r1 + mailbox_tcb], r0
			if r0, !=, 0
				vp_cpy_cl 0, [r1 + mailbox_tcb]
				s_call sys_task, resume, {r0}
			endif
		else
			;going off chip
			s_bind sys_mail, statics, r1
			vp_cpy [r2 + msg_length], r0
			if r0, >, msg_size
				;must use postman task
				vp_cpy [r1 + ml_statics_out_mailbox], r1
				vp_jmp post_it
			else
				;queue it on the outgoing packet list
				vp_lea [r1 + ml_statics_offchip_list], r1
				lh_add_at_tail r1, r2, r0
			endif
		endif
		vp_ret

	fn_function_end
