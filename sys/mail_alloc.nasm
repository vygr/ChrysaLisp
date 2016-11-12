%include 'inc/func.ninc'
%include 'inc/mail.ninc'

def_func sys/mail_alloc
	;outputs
	;r0 = mail message
	;trashes
	;r1-r3

	f_bind sys_mail, statics, r0
	vp_add ml_statics_heap, r0
	f_call sys_heap, alloc, {r0}, {r1}
	vp_cpy r0, [r1]
	vp_lea [r1 + ptr_size], r0
	vp_cpy msg_header_size, r1
	vp_cpy r1, [r0 + msg_length]
	vp_xor r1, r1
	vp_cpy r1, [r0 + msg_parcel_size]
	vp_ret

def_func_end
