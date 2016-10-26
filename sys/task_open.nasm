%include 'inc/func.inc'
%include 'inc/task.inc'

def_func sys/task_open
	;inputs
	;r0 = name string object
	;outputs
	;r0, r1 = mailbox id
	;trashes
	;all but r4

	f_bind sys_task, statics, r1
	f_jmp sys_task, open_device, {r0, [r1 + tk_statics_cpu_id]}

def_func_end
