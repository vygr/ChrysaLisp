%include 'inc/func.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/test10
		;pipe task started by test9

		;read exit command etc
		static_call mail, mymail
		static_call mem, free

		;wait a bit
		vp_cpy 1000000, r0
		fn_call sys/math_random
		vp_add 1000000, r0
		static_call task, sleep

		;print Hello and return
		fn_debug Hello from pipe worker !
		vp_ret

	fn_function_end
