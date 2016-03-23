%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_start, no_debug_enter
		;inputs
		;r0 = new task program counter
		;outputs
		;r0 = new task control block
		;r1 = new task mailbox
		;trashes
		;r2-r3, r5

		;save prog counter
		vp_cpy r0, r5

		;increment task count
		static_bind task, statics, r0
		vp_cpy [r0 + tk_statics_task_count], r1
		vp_inc r1
		vp_cpy r1, [r0 + tk_statics_task_count]

		;create new task control block and task
		vp_add tk_statics_task_heap, r0
		static_call heap, alloc
		vp_cpy r1, r0

		;initialise task mailbox etc
		vp_cpy_cl 0, [r0 + tk_node_node]
		vp_cpy_cl 0, [r0 + tk_node_mailbox + ml_mailbox_tcb]
		vp_lea [r0 + tk_node_mailbox + ml_mailbox_list], r1
		ml_init r1, r2, r3

		;set task control block stack and return address's
		vp_lea [r0 + tk_node_size], r1
		vp_sub tk_state_size + 16, r1
		vp_cpy r1, [r0 + tk_node_stack]
		fn_bind sys/task_stop, r2
		vp_cpy r2, [r1 + tk_state_size + 8]
		vp_cpy r5, [r1 + tk_state_size]

		;resume new task
		static_call task, resume

		;return mailbox pointer
		vp_lea [r0 + tk_node_mailbox], r1
		vp_ret

	fn_function_end
