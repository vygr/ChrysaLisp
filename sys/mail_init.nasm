%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/mail_init, no_debug_enter
		;inputs
		;r0 = kernel mailbox

		;save kernel mailbox
		static_bind mail, statics, r7
		vp_cpy r0, [r7 + ml_statics_kernel_mailbox]

		;init off chip list
		vp_lea [r7 + ml_statics_offchip_list], r0
		lh_init r0, r1

		;init mail message heap
		vp_lea [r7 + ml_statics_heap], r0
		vp_cpy ml_msg_size + 8, r1
		vp_cpy (ml_msg_size + 8) * 256, r2
		static_call heap, init

		;init in and out postmen tasks
		static_bind mail, in, r0
		static_call task, start
		vp_cpy r0, [r7 + ml_statics_in_mailbox]
		static_bind mail, out, r0
		static_call task, start
		vp_cpy r0, [r7 + ml_statics_out_mailbox]
		vp_cpy_cl 0, [r7 + ml_statics_parcel_id]
		vp_ret

	fn_function_end
