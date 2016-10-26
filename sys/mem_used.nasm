%include 'inc/func.inc'
%include 'inc/heap.inc'

def_func sys/mem_used
	;outputs
	;r0 = amount in bytes

	f_bind sys_mem, statics, r0
	vp_cpy [r0], r0
	vp_ret

def_func_end
