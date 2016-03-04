%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_suspend, no_debug_enter
		;push task state
		tk_save_state

		;save stack pointer
		fn_bind sys/task_statics, r0
		vp_cpy [r0 + tk_statics_current_tcb], r0
		vp_cpy r4, [r0 + tk_node_stack]

		;remove task control block
		ln_remove_node r0, r15

		;restore next task
		fn_jmp sys/task_restore

	fn_function_end
