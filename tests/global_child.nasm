%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/math.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	TEST_SIZE equ 1000

	fn_function tests/global_child

		;wait a bit
		static_call sys_math, random, {1000000}
		vp_add 1000000, r0
		static_call sys_task, sleep

		;read mail commands
		for r14, 0, 10, 1
			static_call sys_mail, mymail
			for r15, 0, TEST_SIZE, 1
				if r15, !=, [r0 + (r15 * 8) + ml_msg_data]
					fn_debug_str 'Failed to verify data !'
					vp_ret
				endif
			next
			static_call sys_mem, free
		next

		fn_debug_str 'Hello from global worker !'
		vp_ret

	fn_function_end
