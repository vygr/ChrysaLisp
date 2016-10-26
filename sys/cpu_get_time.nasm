%include 'inc/func.inc'
%include 'inc/syscall.inc'

def_func sys/cpu_get_time
	;outputs
	;r0 = time in usec

	;calculate wake time
	vp_sub timeval_size, r4
	vp_cpy r4, r0
	sys_gettimeofday r0, 0
	vp_cpy [r4 + timeval_sec], r0
	vp_mul 1000000, r0
	vp_add [r4 + timeval_usec], r0
	vp_add timeval_size, r4
	vp_ret

def_func_end
