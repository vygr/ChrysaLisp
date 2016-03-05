%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_out, no_debug_enter
		;parcels going off chip task

		loop_start
			;read parcel
			class_call mail, mymail
			vp_cpy r0,r15

			;create next parcel id
			fn_call sys/cpu_get_id
			vp_cpy r0, r6
			class_bind mail, statics, r1
			vp_cpy [r1 + ml_statics_parcel_id], r7
			vp_inc r7
			vp_cpy r7, [r1 + ml_statics_parcel_id]

			;header info for each fragment
			vp_cpy ml_msg_data, r10
			vp_cpy [r15 + ml_msg_length], r11
			vp_cpy [r15 + ml_msg_dest], r12
			vp_cpy [r15 + ml_msg_dest + 8], r13
			loop_start
				;create fragment
				class_call mail, alloc
				fn_assert r0, !=, 0
				vp_cpy r0, r14

				;fill in fragment header
				vp_cpy r12, [r14 + ml_msg_dest]
				vp_cpy r13, [r14 + ml_msg_dest + 8]
				vp_cpy r10, [r14 + ml_msg_parcel_frag]
				vp_cpy r11, [r14 + ml_msg_parcel_size]
				vp_cpy r6, [r14 + ml_msg_parcel_id]
				vp_cpy r7, [r14 + ml_msg_parcel_id + 8]

				;data source and destination
				vp_lea [r15 + r10], r0
				vp_lea [r14 + ml_msg_data], r1

				;length of fragment data
				vp_cpy ml_msg_size - ml_msg_data, r2
				vp_add r2, r10
				if r10, >, r11
					vp_sub r11, r10
					vp_sub r10, r2
					vp_cpy r11, r10
				endif
				vp_lea [r2 + ml_msg_data], r3
				vp_cpy r3, [r14 + ml_msg_length]

				;copy data block, round up for speed
				vp_add 7, r2
				vp_and -8, r2
				fn_call sys/mem_copy

				;queue it on the outgoing packet list
				class_bind mail, statics, r0
				vp_lea [r0 + ml_statics_offchip_list], r0
				lh_add_at_tail r0, r14, r1
			loop_until r10, ==, r11

			;free parcel
			vp_cpy r15, r0
			fn_call sys/mem_free

			;let links get at some packets
			class_call task, yield
		loop_end
		vp_ret

	fn_function_end
