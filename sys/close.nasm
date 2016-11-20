%include 'inc/func.ninc'
%include 'inc/syscall.ninc'

def_func sys/close
	;inputs
	;r0 = fd

	sys_close r0
	vp_ret

def_func_end
