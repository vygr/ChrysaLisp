%include "func.inc"
%include "mail.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	TEST_SIZE equ 1000

	fn_function "tests/test12"
		;pipe task started by test11

		;wait a bit
		vp_cpy 12000000, r0
		fn_call sys/task_sleep

		;read 10 mail commands
		for r14, 0, 10, 1
			fn_call sys/mail_read_mymail
			for r15, 0, TEST_SIZE, 1
				if r15, !=, [r0 + (r15 * 8) + ML_MSG_DATA]
					vp_lea [rel failed], r0
					vp_cpy 1, r1
					fn_jmp sys/write_string
				endif
			next
			fn_call sys/mem_free
		next

		;print Hello and return
		vp_lea [rel hello], r0
		vp_cpy 1, r1
		fn_jmp sys/write_string

	hello:
		db "Hello from global worker !", 10, 0
	failed:
		db "Failed to verify data !", 10, 0

	fn_function_end
