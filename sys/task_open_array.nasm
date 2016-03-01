%include "inc/func.inc"
%include "inc/mail.inc"

	fn_function "sys/task_open_array"
		;inputs
		;r0 = new task function names
		;r1 = mailbox array pointer
		;trashes
		;r0-r3, r5-r7

		;save task info
		vp_cpy r0, r5
		vp_cpy r1, r6
		vp_xor r7, r7

		;create temp mailbox
		ml_temp_create r0, r1

		;start all tasks
		loop_start
			;allocate mail message
			fn_call sys/mail_alloc
			vp_cpy r0, r3

			;fill in destination, reply, function and user
			fn_call sys/cpu_get_id
			vp_cpy 0, qword[r3 + ML_MSG_DEST]
			vp_cpy r0, [r3 + (ML_MSG_DEST + 8)]
			vp_cpy r4, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY)]
			vp_cpy r0, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY + 8)]
			vp_cpy r6, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_USER)]
			vp_cpy KN_CALL_TASK_CHILD, qword[r3 + (ML_MSG_DATA + KN_DATA_KERNEL_FUNCTION)]

			;copy task name, move to next task name
			vp_cpy r5, r0
			vp_lea [r3 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_PATHNAME)], r1
			fn_call sys/string_copy
			vp_cpy r0, r5

			;fill in total message length
			vp_sub r3, r1
			vp_cpy r1, [r3 + ML_MSG_LENGTH]

			;send mail to kernel
			vp_cpy r3, r0
			fn_call sys/mail_send

			;next array worker
			vp_add 16, r6
			vp_inc r7
			vp_cpy byte[r5], r0l
			vp_and 0xff, r0
		loop_until r0, ==, 0

		;wait for all replies
		loop_start
			vp_cpy r4, r0
			fn_call sys/mail_read

			;save reply mailbox ID in user address
			vp_cpy [r0 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_USER)], r6
			vp_cpy [r0 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_MAILBOXID)], r2
			vp_cpy [r0 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_MAILBOXID + 8)], r3
			vp_cpy r2, [r6]
			vp_cpy r3, [r6 + 8]

			;free reply mail
			fn_call sys/mem_free

			;next mailbox
			vp_dec r7
		loop_until r7, ==, 0

		;free temp mailbox
		ml_temp_destroy
		vp_ret

	fn_function_end
