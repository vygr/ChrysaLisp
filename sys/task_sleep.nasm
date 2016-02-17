%include "func.inc"
%include "task.inc"

	fn_function "sys/task_sleep"
		;inputs
		;r0 = time delay in usec

		;push task state
		tk_save_state

		;save stack pointer
		fn_bind sys/task_statics, r3
		vp_cpy [r3 + TK_STATICS_CURRENT_TCB], r15
		vp_cpy r4, [r15 + TK_NODE_STACK]

		;save timeout
		vp_cpy r0, r1

		;calculate wake time
		fn_call sys/get_cpu_time
		vp_add r1, r0
		vp_cpy r0, [r15 + TK_NODE_TIME]

		;remove task control block
		vp_cpy r15, r2
		vp_cpy r15, r1
		ln_remove_node r2, r15

		;add to timer list
		loopstart_list_forwards r3 + TK_STATICS_TIMER_LIST, r2, r5
		until r0, <, [r5 + TK_NODE_TIME]
		ln_add_node_before r5, r1, r0

		;restore next task
		fn_jmp sys/task_restore

	fn_function_end
