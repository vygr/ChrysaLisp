%include "func.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test3"
		;task started by test1

		;read and free 1000000 messages
		for r8, 0, 1000000, 1
			vp_lea [r15 + TK_NODE_MAILBOX], r0
			kn_call KERNEL_READ_MAIL
			kn_call KERNEL_FREE_MAIL
		next

		for r10, 0, 10, 1
			for r9, 0, 10, 1
				for r8, 0, 10, 1
					vp_cpy r8, r0
					vp_add r9,r0
					vp_add 'a', r0
					vp_cpy 1, r1
					kn_call KERNEL_PRINT_CHAR
					vp_cpy ' ', r0
					kn_call KERNEL_PRINT_CHAR
				next
				vp_cpy 10, r0
				kn_call KERNEL_PRINT_CHAR
			next
			kn_call KERNEL_PRINT_CHAR
			kn_call KERNEL_DESHEDULE_TASK
		next

		;stop this task
		kn_call KERNEL_STOP_TASK

	fn_function_end
