%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_out"
		;parcels going off chip task

		loop_start
			;read parcel
			fn_call sys/mail_read_mymail
			vp_cpy r0,r15

			;create next parcel id
			fn_call sys/get_cpu_id
			vp_cpy r0, r6
			fn_bind sys/mail_statics, r1
			vp_cpy [r1 + ML_STATICS_PARCEL_ID], r7
			vp_inc r7
			vp_cpy r7, [r1 + ML_STATICS_PARCEL_ID]

			;header info for each fragment
			vp_cpy ML_MSG_DATA, r10
			vp_cpy [r15 + ML_MSG_LENGTH], r11
			vp_cpy [r15 + ML_MSG_DEST], r12
			vp_cpy [r15 + ML_MSG_DEST + 8], r13
			loop_start
				;create fragment
				fn_call sys/mail_alloc
				vp_cpy r0, r14

				;fill in fragment header
				vp_cpy r12, [r14 + ML_MSG_DEST]
				vp_cpy r13, [r14 + ML_MSG_DEST + 8]
				vp_cpy r10, [r14 + ML_MSG_PARCEL_FRAG]
				vp_cpy r11, [r14 + ML_MSG_PARCEL_SIZE]
				vp_cpy r6, [r14 + ML_MSG_PARCEL_ID]
				vp_cpy r7, [r14 + ML_MSG_PARCEL_ID + 8]

				;data source and destination
				vp_lea [r15 + r10], r0
				vp_lea [r14 + ML_MSG_DATA], r1

				;length of fragment data
				vp_cpy ML_MSG_SIZE - ML_MSG_DATA, r2
				vp_add r2, r10
				if r10, >, r11
					vp_sub r11, r10
					vp_sub r10, r2
					vp_cpy r11, r10
				endif
				vp_lea [r2 + ML_MSG_DATA], r3
				vp_cpy r3, [r14 + ML_MSG_LENGTH]

				;copy data block
				fn_call sys/mem_copy

				;queue it on the outgoing packet list
				fn_bind sys/mail_statics, r0
				vp_lea [r0 + ML_STATICS_OFFCHIP_LIST], r0
				lh_add_at_tail r0, r14, r1
			loop_until r10, ==, r11

			;free parcel
			vp_cpy r15, r0
			fn_call sys/mem_free

			;let links get at some packets
			fn_call sys/task_yield
		loop_end
		vp_ret

	fn_function_end
