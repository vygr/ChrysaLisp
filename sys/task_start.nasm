%include "func.inc"
%include "task.inc"

	fn_function "sys/task_start"
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
		fn_bind sys/task_statics, r6

		;increment task count
		vp_cpy [r6 + TK_STATICS_TASK_COUNT], r0
		vp_inc r0
		vp_cpy r0, [r6 + TK_STATICS_TASK_COUNT]

		;create new task control block and task
		vp_lea [r6 + TK_STATICS_TASK_HEAP], r0
		fn_call sys/heap_alloccell
		vp_cpy [r6 + TK_STATICS_CURRENT_TCB], r0
		ln_add_node_before r0, r1, r2

		;initialise task mailbox
		vp_cpy 0, qword[r1 + TK_NODE_MAILBOX + ML_MAILBOX_TCB]
		vp_lea [r1 + TK_NODE_MAILBOX + ML_MAILBOX_LIST], r0
		ml_init r0, r2, r3

		;set task control block stack and return address's
		vp_lea [r1 + TK_NODE_SIZE], r0
		vp_sub TK_STATE_SIZE + 16, r0
		vp_cpy r0, [r1 + TK_NODE_STACK]
		fn_bind sys/task_stop, r2
		vp_cpy r2, [r0 + TK_STATE_SIZE + 8]
		vp_cpy r5, [r0 + TK_STATE_SIZE]

		;return mailbox pointer
		vp_lea [r1 + TK_NODE_MAILBOX], r0
		vp_ret

	fn_function_end
