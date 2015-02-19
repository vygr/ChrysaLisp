%include "func.inc"
%include "task.inc"

	fn_function "sys/task_start"
		;inputs
		;r0 = new task program counter
		;outputs
		;r0 = new task mailbox
		;trashes
		;r1-r3, r5-r6

		;save prog counter
		vp_cpy r0,r5

		;get statics
		fn_bind sys/task_statics, r0
		vp_cpy	r0, r6

		;create new task control block and task
		vp_lea [r6 + TK_STATICS_TASK_HEAP], r0
		fn_call sys/heap_alloccell
		vp_lea [r6 + TK_STATICS_TASK_LIST], r0
		lh_add_at_head r0, r1, r2

		;initialise task mailbox
		vp_cpy 0, long[r1 + TK_NODE_MAILBOX + ML_MAILBOX_TCB]
		vp_lea [r1 + TK_NODE_MAILBOX + ML_MAILBOX_LIST], r0
		ml_init r0, r2

		;set task control block stack and return address
		vp_lea [r1 + TK_NODE_SIZE], r0
		vp_sub 8, r0
		vp_cpy r5, [r0]
		vp_sub 14*8, r0
		vp_cpy r0, [r1 + TK_NODE_STACK]

		;return mailbox pointer
		vp_lea [r1 + TK_NODE_MAILBOX], r0
		vp_ret

	fn_function_end
