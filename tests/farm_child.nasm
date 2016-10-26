%include 'inc/func.inc'
%include 'inc/math.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

def_func tests/farm_child

	ptr msg
	ulong num

	push_scope

	;read exit command etc
	func_call sys_mail, mymail, {}, {msg}
	func_call sys_mem, free, {msg}

	;wait a bit
	func_call sys_math, random, {1000000}, {num}
	func_call sys_task, sleep, {num + 1000000}

	pop_scope
	return

def_func_end
