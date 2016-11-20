%include 'inc/func.ninc'
%include 'inc/syscall.ninc'

def_func sys/open
	;inputs
	;r0 = filename
	;r1 = mode
	;r2 = flags
	;outputs
	;r0 = fd

	sys_open r0, r1, r2
	vp_ret

def_func_end
