%include 'inc/func.inc'
%include 'inc/math.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/array_child

		;read exit command etc
		static_call sys_mail, mymail, {}, {r0}
		static_call sys_mem, free, {r0}

		;wait a bit
		static_call sys_math, random, {1000000}, {r0}
		vp_add 1000000, r0
		static_call sys_task, sleep, {r0}

		;print Hello and return
		fn_debug_str 'Hello from array worker !'
		vp_ret

	fn_function_end
