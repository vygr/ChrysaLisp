%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_alloc
		;outputs
		;r0 = mail message
		;trashes
		;r1-r3

		static_bind sys_mail, statics, r0
		vp_add ml_statics_heap, r0
		s_call sys_heap, alloc, {r0}, {r1}
		vp_cpy r0, [r1]
		vp_lea [r1 + 8], r0
		vp_cpy_cl ml_msg_data, [r0 + ml_msg_length]
		vp_cpy_cl 0, [r0 + ml_msg_parcel_size]
		vp_ret

	fn_function_end
