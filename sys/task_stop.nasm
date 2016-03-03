%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function "sys/task_stop"
		;remove task control block
		fn_bind sys/task_statics, r0
		vp_cpy [r0 + TK_STATICS_CURRENT_TCB], r1
		vp_cpy r1, r2
		ln_remove_node r2, r15

		;decrement task count
		vp_cpy [r0 + TK_STATICS_TASK_COUNT], r2
		vp_dec r2
		vp_cpy r2, [r0 + TK_STATICS_TASK_COUNT]

		;free our task control block
		vp_lea [r0 + TK_STATICS_TASK_HEAP], r0
		hp_freecell r0, r1, r2
		fn_jmp sys/task_restore

	fn_function_end
