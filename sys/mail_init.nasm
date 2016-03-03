%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_init
		;inputs
		;r0 = kernel mailbox

		;save kernel mailbox
		fn_bind sys/mail_statics, r7
		vp_cpy r0, [r7 + ml_statics_kernel_mailbox]

		;init off chip list
		vp_lea [r7 + ml_statics_offchip_list], r0
		lh_init r0, r1

		;init mail message heap
		vp_lea [r7 + ml_statics_heap], r0
		vp_cpy ml_msg_size + 8, r1
		vp_cpy (ml_msg_size + 8) * 256, r2
		fn_call sys/heap_init

		;init in and out postmen tasks
		fn_bind sys/mail_in, r0
		fn_call sys/task_start
		vp_cpy r0, [r7 + ml_statics_in_mailbox]
		fn_bind sys/mail_out, r0
		fn_call sys/task_start
		vp_cpy r0, [r7 + ml_statics_out_mailbox]
		vp_cpy 0, qword[r7 + ml_statics_parcel_id]
		vp_ret

	fn_function_end
