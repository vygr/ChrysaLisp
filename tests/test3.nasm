%include 'inc/func.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/test3
		;task started by test1

		;read and free 1000 messages
		for r14, 0, 1000, 1
			class_call mail, mymail
			class_call mem, free
		next

		for r14, 0, 2, 1
			for r13, 0, 10, 1
				for r12, 0, 10, 1
					vp_cpy r12, r0
					vp_add r13,r0
					vp_add 'a', r0
					vp_cpy 1, r1
					class_call io, char
					vp_cpy ' ', r0
					class_call io, char
				next
				vp_cpy 10, r0
				class_call io, char
			next
			class_call io, char
			class_call task, yield
		next
		vp_ret

	fn_function_end
