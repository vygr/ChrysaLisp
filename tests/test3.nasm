%include "func.inc"
%include "task.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test3"
		;task started by test1

		;read and free 1000 messages
		for r14, 0, 1000, 1
			vp_lea [r15 + TK_NODE_MAILBOX], r0
			fn_call sys/mail_read
			fn_call sys/mail_free
		next

		for r14, 0, 2, 1
			for r13, 0, 10, 1
				for r12, 0, 10, 1
					vp_cpy r12, r0
					vp_add r13,r0
					vp_add 'a', r0
					vp_cpy 1, r1
					fn_call sys/write_char
					vp_cpy ' ', r0
					fn_call sys/write_char
				next
				vp_cpy 10, r0
				fn_call sys/write_char
			next
			fn_call sys/write_char
			fn_call sys/task_deshedule
		next

		;stop this task
		fn_jmp sys/task_stop

	fn_function_end
