%include 'inc/func.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/test8
		;array task started by test7

		;read exit command etc
		class_call mail, mymail
		class_call mem, free

		;wait a bit
		vp_cpy 2000000, r0
		fn_call sys/math_random
		vp_add 6000000, r0
		class_call task, sleep

		;print Hello and return
		vp_lea [rel hello], r0
		vp_cpy 1, r1
		fn_jmp sys/write_string

	hello:
		db 'Hello from array worker !', 10, 0

	fn_function_end
