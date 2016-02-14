%include "func.inc"

	fn_function "sys/task_open_child"
		;inputs
		;r0 = new task function name
		;outputs
		;r0, r1 = new task mailbox ID
		;trashes
		;r2-r3, r5-r6

		;save name
		vp_cpy r0, r2

		;get local cpu id
		fn_call sys/get_cpu_id
		vp_cpy r0, r1

		;restore name and launch task
		vp_cpy r2, r0
		fn_jmp sys/task_open_remote

	fn_function_end
