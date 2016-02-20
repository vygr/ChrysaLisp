%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_alloc_parcel"
		;inputs
		;r0 = parcel data size
		;outputs
		;r0 = mail message
		;trashes
		;r1-r3, r5-r6

		vp_add ML_MSG_DATA, r0
		vp_cpy r0, r6
		fn_call sys/mem_alloc
		vp_cpy r6, qword[r0 + ML_MSG_LENGTH]
		vp_cpy 0, qword[r0 + ML_MSG_PARCEL_SIZE]
		vp_ret

	fn_function_end
