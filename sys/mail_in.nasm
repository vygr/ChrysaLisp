%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_in"
		;parcel fragments arriving on chip task

		loop_start
			;read parcel fragment
			fn_call sys/mail_read_mymail
			vp_cpy r0, r15

			;look up parcel in mailbox
			vp_cpy [r15 + ML_MSG_PARCEL_ID], r6
			vp_cpy [r15 + ML_MSG_PARCEL_ID + 8], r7
			vp_cpy [r15 + ML_MSG_DEST], r13
			loop_list_forwards r13 + ML_MAILBOX_PARCEL_LIST, r1, r0
				continueif r6, !=, [r0 + ML_MSG_PARCEL_ID]
			loop_until r7, ==, [r0 + ML_MSG_PARCEL_ID + 8]
			if r1, ==, 0
				;new parcel
				vp_cpy [r15 + ML_MSG_PARCEL_SIZE], r12
				vp_cpy [r15 + ML_MSG_DEST + 8], r14
				vp_cpy r12, r0
				fn_call sys/mem_alloc
				vp_cpy r12, [r0 + ML_MSG_LENGTH]
				vp_cpy r13, [r0 + ML_MSG_DEST]
				vp_cpy r14, [r0 + ML_MSG_DEST + 8]
				vp_cpy r6, [r0 + ML_MSG_PARCEL_ID]
				vp_cpy r7, [r0 + ML_MSG_PARCEL_ID + 8]
				vp_cpy ML_MSG_DATA, qword[r0 + ML_MSG_PARCEL_TOTAL]
				vp_cpy 0, qword[r0 + ML_MSG_PARCEL_SIZE]
				vp_add ML_MAILBOX_PARCEL_LIST, r13
				lh_add_at_tail r13, r0, r1
			endif
			vp_cpy r0, r14

			;destination address
			vp_cpy r14, r1
			vp_add [r15 + ML_MSG_PARCEL_FRAG], r1

			;source address
			vp_lea [r15 + ML_MSG_DATA], r0

			;fragment size
			vp_cpy [r15 + ML_MSG_LENGTH], r2
			vp_sub ML_MSG_DATA, r2

			;total so far
			vp_cpy [r14 + ML_MSG_PARCEL_TOTAL], r13
			vp_add r2, r13

			;copy fragment data
			fn_call sys/mem_copy

			;got all needed ?
			if r13, ==, [r14 + ML_MSG_LENGTH]
				;yes, remove parcel and post it
				vp_cpy r14, r1
				ln_remove_node r1, r2
				vp_cpy r14, r0
				fn_call sys/mail_send
			else
				;no, update total so far
				vp_cpy r13, [r14 + ML_MSG_PARCEL_TOTAL]
			endif

			;free fragment
			vp_cpy r15, r0
			fn_call sys/mem_free
		loop_end
		vp_ret

	fn_function_end
