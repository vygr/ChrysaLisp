%include 'inc/func.inc'
%include 'inc/math.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	def_function tests/pipe_child

		ptr msg
		ulong num

		push_scope

		;read exit command etc
		static_call sys_mail, mymail, {}, {msg}
		static_call sys_mem, free, {msg}

		;wait a bit
		static_call sys_math, random, {1000000}, {num}
		static_call sys_task, sleep, {num + 1000000}

		pop_scope
		return

	def_function_end
