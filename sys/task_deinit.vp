%include 'inc/func.ninc'
%include 'inc/task.ninc'

def_func sys/task_deinit

	;free the task heap
	f_bind sys_task, statics, r0
	f_jmp sys_heap, deinit, {&[r0 + tk_statics_task_heap]}

def_func_end
