%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_alloc, no_debug_enter
		;outputs
		;r0 = mail message
		;trashes
		;r1-r3

		class_bind mail, statics, r0
		vp_lea [r0 + ml_statics_heap], r0
		fn_call sys/heap_alloccell
		vp_cpy r0, qword[r1]
		vp_lea [r1 + 8], r0
		vp_cpy ml_msg_data, qword[r0 + ml_msg_length]
		vp_cpy 0, qword[r0 + ml_msg_parcel_size]
		vp_ret

	fn_function_end
