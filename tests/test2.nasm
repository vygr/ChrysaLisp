%include "func.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test2"
		;function called by test1
		;trashes
		;r0-r3, r5

		for r10, 0, 10, 1
			for r9, 0, 10, 1
				for r8, 0, 10, 1
					vp_cpy r8, r11
					vp_mul r9, r11
					if r11, < , 10
						vp_cpy ' ', r0
						vp_cpy 1, r1
						kn_call KERNEL_PRINT_CHAR
					endif
					vp_cpy r11, r0
					vp_cpy 1, r1
					kn_call KERNEL_PRINT_NUMBER
					vp_cpy ' ', r0
					vp_cpy 1, r1
					kn_call KERNEL_PRINT_CHAR
				next
				vp_cpy 10, r0
				kn_call KERNEL_PRINT_CHAR
			next
			kn_call KERNEL_PRINT_CHAR
			kn_call KERNEL_DESHEDULE_TASK
		next
		vp_ret

	fn_function_end
