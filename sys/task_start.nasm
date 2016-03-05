%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_start, no_debug_enter
		;inputs
		;r0 = new task program counter
		;outputs
		;r0 = new task mailbox
		;r1 = new task control block
		;trashes
		;r2-r3, r5-r6

		;save prog counter
		vp_cpy r0, r5

		;get statics
		class_bind task, statics, r6

		;increment task count
		vp_cpy [r6 + tk_statics_task_count], r0
		vp_inc r0
		vp_cpy r0, [r6 + tk_statics_task_count]

		;create new task control block and task
		vp_lea [r6 + tk_statics_task_heap], r0
		class_call heap, alloc
		vp_cpy [r6 + tk_statics_current_tcb], r0
		ln_add_node_before r0, r1, r2

		;initialise task mailbox
		vp_cpy 0, qword[r1 + tk_node_mailbox + ml_mailbox_tcb]
		vp_lea [r1 + tk_node_mailbox + ml_mailbox_list], r0
		ml_init r0, r2, r3

		;set task control block stack and return address's
		vp_lea [r1 + tk_node_size], r0
		vp_sub tk_state_size + 16, r0
		vp_cpy r0, [r1 + tk_node_stack]
		fn_bind sys/task_stop, r2
		vp_cpy r2, [r0 + tk_state_size + 8]
		vp_cpy r5, [r0 + tk_state_size]

		;return mailbox pointer
		vp_lea [r1 + tk_node_mailbox], r0
		vp_ret

	fn_function_end
