%include 'inc/func.inc'
%include 'inc/mail.inc'

def_func sys/mail_alloc_parcel
	;inputs
	;r0 = parcel size
	;outputs
	;r0 = mail message
	;trashes
	;r1-r3

	vp_cpy r0, r5
	if r0, <=, msg_size
		f_call sys_mail, alloc, {}, {r0}
	else
		f_call sys_mem, alloc, {r0}, {r0, _}
		vp_cpy_cl 0, [r0 + msg_parcel_size]
	endif
	vp_cpy r5, [r0 + msg_length]
	vp_ret

def_func_end
