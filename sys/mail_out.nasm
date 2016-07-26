%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_out
		;parcels going off chip task

		loop_start
			;read parcel
			s_call sys_mail, mymail, {}, {r15}

			;create next parcel id
			s_call sys_cpu, id, {}, {r6}
			static_bind sys_mail, statics, r1
			vp_cpy [r1 + ml_statics_parcel_id], r7
			vp_inc r7
			vp_cpy r7, [r1 + ml_statics_parcel_id]

			;header info for each fragment
			vp_cpy msg_data, r10
			vp_cpy [r15 + msg_length], r11
			vp_cpy [r15 + msg_dest], r12
			vp_cpy [r15 + msg_dest + 8], r13
			loop_start
				;create fragment
				s_call sys_mail, alloc, {}, {r14}
				assert r0, !=, 0

				;fill in fragment header
				vp_cpy r12, [r14 + msg_dest]
				vp_cpy r13, [r14 + msg_dest + 8]
				vp_cpy r10, [r14 + msg_parcel_frag]
				vp_cpy r11, [r14 + msg_parcel_size]
				vp_cpy r6, [r14 + msg_parcel_id]
				vp_cpy r7, [r14 + msg_parcel_id + 8]

				;data source and destination
				vp_lea [r15 + r10], r0
				vp_lea [r14 + msg_data], r1

				;length of fragment data
				vp_cpy msg_size - msg_data, r2
				vp_add r2, r10
				if r10, >, r11
					vp_sub r11, r10
					vp_sub r10, r2
					vp_cpy r11, r10
				endif
				vp_lea [r2 + msg_data], r3
				vp_cpy r3, [r14 + msg_length]

				;copy data block, round up for speed
				vp_add ptr_size - 1, r2
				vp_and -ptr_size, r2
				s_call sys_mem, copy, {r0, r1, r2}, {_, _}

				;queue it on the outgoing packet list
				static_bind sys_mail, statics, r0
				vp_lea [r0 + ml_statics_offchip_list], r0
				lh_add_at_tail r0, r14, r1

				;let links get at some packets
				s_call sys_task, yield
			loop_until r10, ==, r11

			;free parcel
			s_call sys_mem, free, {r15}
		loop_end
		vp_ret

	fn_function_end
