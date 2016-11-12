%include 'inc/func.ninc'
%include 'inc/mail.ninc'

def_func sys/mail_init
	;inputs
	;r1 = kernel mailbox

	;save kernel mailbox
	f_bind sys_mail, statics, r7
	vp_cpy r1, [r7 + ml_statics_kernel_mailbox]

	;init off chip list
	vp_lea [r7 + ml_statics_offchip_list], r0
	lh_init r0, r1

	;init mail message heap
	f_call sys_heap, init, {&[r7 + ml_statics_heap], (msg_size + ptr_size), ((msg_size + ptr_size) * 16)}

	;init in and out postmen tasks
	func_path sys_mail, in
	f_call sys_task, start, {@_function_}, {r0, [r7 + ml_statics_in_mailbox]}
	func_path sys_mail, out
	f_call sys_task, start, {@_function_}, {r0, [r7 + ml_statics_out_mailbox]}
	vp_xor r1, r1
	vp_cpy r1, [r7 + ml_statics_parcel_id]
	vp_ret

def_func_end
