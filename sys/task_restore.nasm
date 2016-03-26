%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_restore, no_debug_enter
		;restore next task
		;r15 = control block to restore

		;round robin past any list head
		ln_get_forward r15, r0
		static_bind sys_task, statics, r0
		vp_cpy r15, [r0 + tk_statics_current_tcb]

		;restore old stack pointer
		vp_cpy [r15 + tk_node_stack], r4

		;pop task state
		tk_load_state
		vp_ret

	fn_function_end
