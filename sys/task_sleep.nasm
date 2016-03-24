%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_sleep, no_debug_enter
		;inputs
		;r0 = time delay in usec

		;push task state
		tk_save_state

		;save stack pointer
		static_bind task, statics, r3
		vp_cpy [r3 + tk_statics_current_tcb], r15
		vp_cpy r4, [r15 + tk_node_stack]

		;save timeout
		vp_cpy r0, r1

		;calculate wake time
		static_call cpu, time
		vp_add r1, r0
		vp_cpy r0, [r15 + tk_node_time]

		;remove task control block
		vp_cpy r15, r2
		vp_cpy r15, r1
		ln_remove_node r2, r15

		;add to timer list
		loop_list_forward r3 + tk_statics_timer_list, r2, r5
		loop_until r0, <, [r5 + tk_node_time]
		ln_add_node_before r5, r1, r0

		;restore next task
		static_jmp task, restore

	fn_function_end
