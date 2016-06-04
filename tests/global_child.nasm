%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/math.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/global_child

		ptr msg
		ulong num

		push_scope

		;wait a bit
		static_call sys_math, random, {1000000}, {num}
		static_call sys_task, sleep, {num + 1000000}

		;read command
		static_call sys_mail, mymail, {}, {msg}
		static_call sys_mem, free, {msg}

		;print Hello and return
		fn_debug_str 'Hello from global worker !'

		pop_scope
		vp_ret

	fn_function_end
