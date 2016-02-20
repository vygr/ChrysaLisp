%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_send"
		;inputs
		;r0 = mail message
		;trashes
		;r0-r2

		;on or off chip ?
		vp_cpy r0, r2
		fn_call sys/get_cpu_id
		if r0, ==, [r2 + (ML_MSG_DEST + 8)]
			;on this chip
			vp_cpy [r2 + ML_MSG_PARCEL_SIZE], r1
			if r1, !=, 0
				;mail for postman !
				fn_bind sys/mail_statics, r1
				vp_cpy [r1 + ML_STATICS_IN_MAILBOX], r1
			else
				vp_cpy [r2 + ML_MSG_DEST], r1
				if r1, ==, 0
					;mail for kernel !
					fn_bind sys/mail_statics, r1
					vp_cpy [r1 + ML_STATICS_KERNEL_MAILBOX], r1
				endif
			endif
		post_it:
			lh_add_at_tail r1, r2, r0
			vp_cpy [r1 + ML_MAILBOX_TCB], r0
			if r0, !=, 0
				vp_cpy 0, qword[r1 + ML_MAILBOX_TCB]
				fn_call sys/task_resume
			endif
		else
			;going off chip
			fn_bind sys/mail_statics, r1
			vp_cpy [r2 + ML_MSG_LENGTH], r0
			if r0, >, ML_MSG_SIZE
				;must use postman task
				vp_cpy [r1 + ML_STATICS_OUT_MAILBOX], r1
				vp_jmp post_it
			else
				;queue it on the outgoing packet list
				vp_lea [r1 + ML_STATICS_OFFCHIP_LIST], r1
				lh_add_at_tail r1, r2, r0
			endif
		endif
		vp_ret

	fn_function_end
