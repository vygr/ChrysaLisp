%include 'inc/func.ninc'
%include 'inc/mail.ninc'

def_func sys/mail_send
	;inputs
	;r0 = mail message
	;trashes
	;r0-r2

	;on or off chip ?
	vp_cpy r0, r2
	f_call sys_cpu, id, {}, {r0}
	if r0, ==, [r2 + msg_dest + id_cpu]
		;on this chip
		vp_cpy [r2 + msg_parcel_size], r1
		if r1, !=, 0
			;mail for postman !
			f_bind sys_mail, statics, r1
			vp_cpy [r1 + ml_statics_in_mailbox], r1
		else
			vp_cpy [r2 + msg_dest + id_mbox], r1
			if r1, ==, 0
				;mail for kernel !
				f_bind sys_mail, statics, r1
				vp_cpy [r1 + ml_statics_kernel_mailbox], r1
			endif
		endif
	post_it:
		lh_add_at_tail r1, r2, r0
		vp_cpy [r1 + mailbox_tcb], r0
		if r0, !=, 0
			vp_cpy_cl 0, [r1 + mailbox_tcb]
			f_call sys_task, resume, {r0}
		endif
	else
		;going off chip
		f_bind sys_mail, statics, r1
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

def_func_end
