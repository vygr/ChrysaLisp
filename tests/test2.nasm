%include 'inc/func.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/test2
		;function called by test1
		;trashes
		;r0-r3, r5

		for r10, 0, 2, 1
			for r9, 0, 10, 1
				for r8, 0, 10, 1
					vp_cpy r8, r11
					vp_mul r9, r11
					if r11, < , 10
						vp_cpy ' ', r0
						vp_cpy 1, r1
						fn_call sys/write_char
					endif
					vp_cpy r11, r0
					vp_cpy 1, r1
					fn_call sys/write_number
					vp_cpy ' ', r0
					vp_cpy 1, r1
					fn_call sys/write_char
				next
				vp_cpy 10, r0
				fn_call sys/write_char
			next
			fn_call sys/write_char
			fn_call sys/task_yield
		next
		vp_ret

	fn_function_end
