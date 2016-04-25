%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_init, no_debug_enter
		;inputs
		;r1 = kernel mailbox

		;save kernel mailbox
		static_bind sys_mail, statics, r7
		vp_cpy r1, [r7 + ml_statics_kernel_mailbox]

		;init off chip list
		vp_lea [r7 + ml_statics_offchip_list], r0
		lh_init r0, r1

		;init mail message heap
		vp_lea [r7 + ml_statics_heap], r0
		static_call sys_heap, init, {r0, (ml_msg_size + 8), ((ml_msg_size + 8) * 256)}

		;init in and out postmen tasks
		static_bind sys_mail, in, r0
		static_call sys_task, start, {r0}, {r0, [r7 + ml_statics_in_mailbox]}
		static_bind sys_mail, out, r0
		static_call sys_task, start, {r0}, {r0, [r7 + ml_statics_out_mailbox]}
		vp_cpy_cl 0, [r7 + ml_statics_parcel_id]
		vp_ret

	fn_function_end
