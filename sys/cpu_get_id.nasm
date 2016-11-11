%include 'inc/func.ninc'
%include 'inc/task.ninc'

def_func sys/cpu_get_id
	;outputs
	;r0 = cpu ID

	f_bind sys_task, statics, r0
	vp_cpy [r0 + tk_statics_cpu_id], r0
	vp_ret

def_func_end
