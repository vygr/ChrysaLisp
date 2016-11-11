%include 'inc/func.ninc'
%include 'inc/syscall.ninc'

def_func sys/write_char
	;inputs
	;r0 = char
	;r1 = fd

	sys_write_char r1, r0
	vp_ret

def_func_end
