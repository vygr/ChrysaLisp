%include 'inc/func.ninc'
%include 'inc/mail.ninc'

def_func sys/mail_in
	;parcel fragments arriving on chip task

	loop_start
		;read parcel fragment
		f_call sys_mail, mymail, {}, {r15}

		;look up parcel in mailbox
		vp_cpy [r15 + msg_parcel_id + id_mbox], r6
		vp_cpy [r15 + msg_parcel_id + id_cpu], r7
		vp_cpy [r15 + msg_dest + id_mbox], r13
		loop_list_forward r13 + mailbox_parcel_list, r0, r1
			continueif r6, !=, [r0 + msg_parcel_id + id_mbox]
		loop_until r7, ==, [r0 + msg_parcel_id + id_cpu]
		vpif r1, ==, 0
			;new parcel
			vp_cpy [r15 + msg_parcel_size], r12
			vp_cpy [r15 + msg_dest + id_cpu], r14
			f_call sys_mem, alloc, {r12}, {r0, _}
			assert r0, !=, 0
			vp_cpy r12, [r0 + msg_length]
			vp_cpy r13, [r0 + msg_dest + id_mbox]
			vp_cpy r14, [r0 + msg_dest + id_cpu]
			vp_cpy r6, [r0 + msg_parcel_id + id_mbox]
			vp_cpy r7, [r0 + msg_parcel_id + id_cpu]
			vp_cpy msg_data, r1
			vp_cpy r1, [r0 + msg_parcel_total]
			vp_xor r1, r1
			vp_cpy r1, [r0 + msg_parcel_size]
			vp_add mailbox_parcel_list, r13
			lh_add_at_tail r13, r0, r1
		endif
		vp_cpy r0, r14

		;destination address
		vp_cpy r14, r1
		vp_add [r15 + msg_parcel_frag], r1

		;source address
		vp_lea [r15 + msg_data], r0

		;fragment size
		vp_cpy [r15 + msg_length], r2
		vp_sub msg_data, r2

		;total so far
		vp_cpy [r14 + msg_parcel_total], r13
		vp_add r2, r13

		;copy fragment data, round up for speed
		vp_add ptr_size - 1, r2
		vp_and -ptr_size, r2
		f_call sys_mem, copy, {r0, r1, r2}, {_, _}

		;got all needed ?
		vpif r13, ==, [r14 + msg_length]
			;yes, remove parcel and post it
			vp_cpy r14, r1
			ln_remove_node r1, r2
			f_call sys_mail, send, {r14}
		else
			;no, update total so far
			vp_cpy r13, [r14 + msg_parcel_total]
		endif

		;free fragment
		f_call sys_mem, free, {r15}
	loop_end
	vp_ret

def_func_end
