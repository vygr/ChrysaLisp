%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_in, no_debug_enter
		;parcel fragments arriving on chip task

		loop_start
			;read parcel fragment
			static_call sys_mail, mymail, {}, {r15}

			;look up parcel in mailbox
			vp_cpy [r15 + ml_msg_parcel_id], r6
			vp_cpy [r15 + ml_msg_parcel_id + 8], r7
			vp_cpy [r15 + ml_msg_dest], r13
			loop_list_forward r13 + ml_mailbox_parcel_list, r0, r1
				continueif r6, !=, [r0 + ml_msg_parcel_id]
			loop_until r7, ==, [r0 + ml_msg_parcel_id + 8]
			if r1, ==, 0
				;new parcel
				vp_cpy [r15 + ml_msg_parcel_size], r12
				vp_cpy [r15 + ml_msg_dest + 8], r14
				static_call sys_mem, alloc, {r12}, {r0, _}
				assert r0, !=, 0
				vp_cpy r12, [r0 + ml_msg_length]
				vp_cpy r13, [r0 + ml_msg_dest]
				vp_cpy r14, [r0 + ml_msg_dest + 8]
				vp_cpy r6, [r0 + ml_msg_parcel_id]
				vp_cpy r7, [r0 + ml_msg_parcel_id + 8]
				vp_cpy_cl ml_msg_data, [r0 + ml_msg_parcel_total]
				vp_cpy_cl 0, [r0 + ml_msg_parcel_size]
				vp_add ml_mailbox_parcel_list, r13
				lh_add_at_tail r13, r0, r1
			endif
			vp_cpy r0, r14

			;destination address
			vp_cpy r14, r1
			vp_add [r15 + ml_msg_parcel_frag], r1

			;source address
			vp_lea [r15 + ml_msg_data], r0

			;fragment size
			vp_cpy [r15 + ml_msg_length], r2
			vp_sub ml_msg_data, r2

			;total so far
			vp_cpy [r14 + ml_msg_parcel_total], r13
			vp_add r2, r13

			;copy fragment data, round up for speed
			vp_add 7, r2
			vp_and -8, r2
			static_call sys_mem, copy, {r0, r1, r2}, {r0, r1}

			;got all needed ?
			if r13, ==, [r14 + ml_msg_length]
				;yes, remove parcel and post it
				vp_cpy r14, r1
				ln_remove_node r1, r2
				static_call sys_mail, send, {r14}
			else
				;no, update total so far
				vp_cpy r13, [r14 + ml_msg_parcel_total]
			endif

			;free fragment
			static_call sys_mem, free, {r15}
		loop_end
		vp_ret

	fn_function_end
