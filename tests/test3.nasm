%include "func.inc"
%include "syscall.inc"

;;;;;;;;;;;;;;;
; test function
;;;;;;;;;;;;;;;

	fn_function "tests/test3"
		;task started by test1

		for r8, 0, 10, 1
			for r9, 0, 10, 1
				vp_cpy r8, r0
				vp_add r9,r0
				vp_add 'a', r0
				sys_write_char 1, r0
				sys_write_char 1, ' '
			next
			sys_write_char 1, 10
		next
		kn_call KERNEL_STOP_TASK

	fn_function_end
