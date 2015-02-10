%include "func.inc"
%include "syscall.inc"

;;;;;;;;;;;;;;;
; test function
;;;;;;;;;;;;;;;

	fn_function "tests/test2"
		;trashes
		;r0-r3, r5

		for r8, 0, 10, 1
			for r9, 0, 10, 1
				vp_cpy r8, r0
				vp_mul r9,r0
				vp_cpy 1, r1
				kn_call KERNEL_PRINT_NUM
				sys_write_char 1, ' '
			next
			sys_write_char 1, 10
		next
		vp_ret
	fn_function_end
