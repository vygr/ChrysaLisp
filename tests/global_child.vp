%include 'inc/func.ninc'
%include 'inc/mail.ninc'
%include 'inc/math.ninc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

def_func tests/global_child

	ptr msg
	ulong num

	push_scope

	;wait a bit
	func_call sys_math, random, {1000000}, {num}
	func_call sys_task, sleep, {num + 1000000}

	;read command
	func_call sys_mail, mymail, {}, {msg}
	func_call sys_mem, free, {msg}

	pop_scope
	return

def_func_end
