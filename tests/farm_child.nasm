%include 'inc/func.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/farm_child

		;read exit command etc
		static_call sys_mail, mymail
		static_call sys_mem, free

		;wait a bit
		vp_cpy 1000000, r0
		fn_call sys/math_random
		vp_add 1000000, r0
		static_call sys_task, sleep

		;print Hello and return
		fn_debug Hello from farm worker !
		vp_ret

	fn_function_end
