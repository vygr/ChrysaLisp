%include "func.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test3"
		;task started by test1

		;read and free 1000000 messages
		for r8, 0, 1000000, 1
			vp_lea [r15 + TK_NODE_MAILBOX], r0
			kn_call KERNEL_MAIL_READ
			kn_call KERNEL_MAIL_FREE
		next

		for r10, 0, 10, 1
			for r9, 0, 10, 1
				for r8, 0, 10, 1
					vp_cpy r8, r0
					vp_add r9,r0
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
			kn_call KERNEL_TASK_DESHEDULE
		next

		;stop this task
		kn_call KERNEL_TASK_STOP

	fn_function_end
