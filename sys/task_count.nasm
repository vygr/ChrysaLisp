%include 'inc/func.ninc'
%include 'inc/task.ninc'

def_func sys/task_count
	;outputs
	;r0 = task count

	f_bind sys_task, statics, r0
	vp_cpy [r0 + tk_statics_task_count], r0
	vp_ret

def_func_end
