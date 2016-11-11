%include 'inc/func.ninc'
%include 'inc/task.ninc'

def_func sys/task_suspend
	;push task state
	tk_save_state

	;save stack pointer
	f_bind sys_task, statics, r0
	vp_cpy [r0 + tk_statics_current_tcb], r0
	vp_cpy r4, [r0 + tk_node_stack]

	;remove task control block, and flag as suspended
	vp_cpy r0, r1
	ln_remove_node r1, r15
	vp_xor r1, r1
	vp_cpy r1, [r0]

	;restore next task
	f_jmp sys_task, restore

def_func_end
