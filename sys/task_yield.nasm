%include 'inc/func.ninc'
%include 'inc/task.ninc'

def_func sys/task_yield
	;push task state
	tk_save_state

	;save old stack pointer
	f_bind sys_task, statics, r15
	vp_cpy [r15 + tk_statics_current_tcb], r15
	vp_cpy r4, [r15 + tk_node_stack]

	;get next task control block
	ln_get_succ r15, 0, r15
	f_jmp sys_task, restore

def_func_end
