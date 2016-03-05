%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_send, no_debug_enter
		;inputs
		;r0 = mail message
		;trashes
		;r0-r2

		;on or off chip ?
		vp_cpy r0, r2
		class_call cpu, id
		if r0, ==, [r2 + (ml_msg_dest + 8)]
			;on this chip
			vp_cpy [r2 + ml_msg_parcel_size], r1
			if r1, !=, 0
				;mail for postman !
				class_bind mail, statics, r1
				vp_cpy [r1 + ml_statics_in_mailbox], r1
			else
				vp_cpy [r2 + ml_msg_dest], r1
				if r1, ==, 0
					;mail for kernel !
					class_bind mail, statics, r1
					vp_cpy [r1 + ml_statics_kernel_mailbox], r1
				endif
			endif
		post_it:
			lh_add_at_tail r1, r2, r0
			vp_cpy [r1 + ml_mailbox_tcb], r0
			if r0, !=, 0
				vp_cpy 0, qword[r1 + ml_mailbox_tcb]
				class_call task, resume
			endif
		else
			;going off chip
			class_bind mail, statics, r1
			vp_cpy [r2 + ml_msg_length], r0
			if r0, >, ml_msg_size
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
