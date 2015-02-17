%include "func.inc"
%include "mail.inc"

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function "tests/test1"
		;task started by test kernel

		;open test9 task, off chip
		vp_lea [rel task_nine], r0
		fn_call sys/task_open_child

		;open test7 task, off chip
		vp_lea [rel task_seven], r0
		fn_call sys/task_open_child

		;open test5 task, off chip
		vp_lea [rel task_five], r0
		fn_call sys/task_open_child

		;open test4 task, on chip
		vp_lea [rel task_four], r0
		fn_call sys/task_open

		;open test3 child task, off chip
		vp_lea [rel task_three], r0
		fn_call sys/task_open_child

		;send test3 task 1000 messages
		vp_cpy r0, r8
		vp_cpy r1, r9
		for r14, 0, 1000, 1
			fn_call sys/mail_alloc
			vp_cpy r8, [r0 + ML_MSG_DEST]
			vp_cpy r9, [r0 + (ML_MSG_DEST + 8)]
			fn_call sys/mail_send
			fn_call sys/task_deshedule
		next

		;make test2 function call
		fn_call tests/test2

		;stop this task
		fn_jmp sys/task_stop

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

	fn_function_end
