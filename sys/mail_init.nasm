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
		static_call sys_heap, init, {:[r7 + ml_statics_heap], (ml_msg_size + 8), ((ml_msg_size + 8) * 256)}

		;init in and out postmen tasks
		slot_function sys_mail, in
		static_call sys_task, start, {@_function_}, {r0, [r7 + ml_statics_in_mailbox]}
		slot_function sys_mail, out
		static_call sys_task, start, {@_function_}, {r0, [r7 + ml_statics_out_mailbox]}
		vp_cpy_cl 0, [r7 + ml_statics_parcel_id]
		vp_ret

	fn_function_end
