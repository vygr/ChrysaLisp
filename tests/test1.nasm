%include 'inc/func.inc'
%include 'inc/task.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/test1
		;task started by test kernel

		;delay for a while to let the network routing finish
		vp_cpy 1000000, r0
		static_call task, sleep

		;open test11 task, remotely
		vp_lea [rel task_eleven], r0
		vp_cpy 0, r1
		static_call task, remote

		;open test9 task, off chip
		vp_lea [rel task_nine], r0
		static_call task, child

		;open test7 task, off chip
		vp_lea [rel task_seven], r0
		static_call task, child

		;open test5 task, off chip
		vp_lea [rel task_five], r0
		static_call task, child

		;open test4 task, on chip
		vp_lea [rel task_four], r0
		static_call task, open

		;open test3 child task, on device
		vp_lea [rel task_three], r0
		vp_cpy 0, r1
		static_call task, device

		;send test3 task 1000 messages
		vp_cpy r0, r8
		vp_cpy r1, r9
		for r14, 0, 1000, 1
			static_call mail, alloc
			fn_assert r0, !=, 0
			vp_cpy r8, [r0 + ml_msg_dest]
			vp_cpy r9, [r0 + (ml_msg_dest + 8)]
			static_call mail, send
			static_call task, yield
		next

		;make test2 function call and return
		fn_jmp tests/test2

	task_three:
		db 'tests/test3', 0
	task_four:
		db 'tests/test4', 0
	task_five:
		db 'tests/test5', 0
	task_seven:
		db 'tests/test7', 0
	task_nine:
		db 'tests/test9', 0
	task_eleven:
		db 'tests/test11', 0

	fn_function_end
