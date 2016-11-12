%include 'inc/func.ninc'
%include 'inc/task.ninc'

def_func sys/task_tcb
	;outputs
	;r0 = current task tcb

	f_bind sys_task, statics, r0
	vp_cpy [r0 + tk_statics_current_tcb], r0
	vp_ret

def_func_end
