%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_alloc_parcel, no_debug_enter
		;inputs
		;r0 = parcel data size
		;outputs
		;r0 = mail message
		;trashes
		;r1-r3, r5

		vp_add ml_msg_data, r0
		vp_cpy r0, r5
		class_call mem, alloc
		fn_assert r0, !=, 0
		vp_cpy r5, qword[r0 + ml_msg_length]
		vp_cpy 0, qword[r0 + ml_msg_parcel_size]
		vp_ret

	fn_function_end
