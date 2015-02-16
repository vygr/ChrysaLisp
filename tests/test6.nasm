%include "func.inc"
%include "task.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test6"
		;farm task started by test5

		;read exit command etc
		vp_lea [r15 + TK_NODE_MAILBOX], r0
		fn_call sys/mail_read
		fn_call sys/mail_free

		;print Hellow
		vp_lea [rel hello], r0
		vp_cpy 1, r1
		fn_call sys/write_string

		;stop this task
		fn_jmp sys/task_stop

	hello:
		db "Hello from farm worker !", 10, 0

	fn_function_end
