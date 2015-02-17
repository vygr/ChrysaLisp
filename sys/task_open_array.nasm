%include "func.inc"
%include "mail.inc"

	fn_function "sys/task_open_array"
		;inputs
		;r0 = new task function names
		;r1 = mailbox array pointer
		;trashes
		;r2-r3, r5-r8

		;save task info
		vp_cpy r0, r6
		vp_cpy r1, r7
		vp_xor r8, r8

		;create temp mailbox
		vp_sub ML_MAILBOX_SIZE, r4
		vp_cpy r4, r5

		;initialise temp mailbox
		vp_cpy 0, long[r5 + ML_MAILBOX_TCB]
		vp_lea [r5 + ML_MAILBOX_LIST], r0
		lh_init r0, r1

		;start all tasks
		repeat
			;allocate mail message
			fn_call sys/mail_alloc
			vp_cpy r0, r3

			;fill in destination, reply, function and user
			fn_call sys/get_cpu_id
			vp_cpy 0, long[r3 + ML_MSG_DEST]
			vp_cpy r0, [r3 + (ML_MSG_DEST + 8)]
			vp_cpy r5, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY)]
			vp_cpy r0, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY + 8)]
			vp_cpy r7, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_USER)]
			vp_cpy KN_CALL_TASK_CHILD, long[r3 + (ML_MSG_DATA + KN_DATA_KERNEL_FUNCTION)]

			;copy task name, move to next task name
			vp_cpy r6, r0
			vp_lea [r3 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_PATHNAME)], r1
			fn_call sys/string_copy
			vp_cpy r0, r6

			;fill in total message length
			vp_sub r3, r1
			vp_cpy r1, [r3 + ML_MSG_LENGTH]

			;send mail to kernel
			vp_cpy r3, r0
			fn_call sys/mail_send

			;next array worker
			vp_add 16, r7
			vp_inc r8
			vp_cpy byte[r6], r0l
			vp_and 0xff, r0
		until r0, ==, 0

		;wait for all replies
		repeat
			vp_cpy r5, r0
			fn_call sys/mail_read

			;save reply mailbox ID in user address
			vp_cpy [r1 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_USER)], r7
			vp_cpy [r1 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_MAILBOXID)], r2
			vp_cpy [r1 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_MAILBOXID + 8)], r3
			vp_cpy r2, [r7]
			vp_cpy r3, [r7 + 8]

			;free reply mail
			fn_call sys/mail_free

			;next mailbox
			vp_dec r8
		until r8, ==, 0

		;free temp mailbox
		vp_add ML_MAILBOX_SIZE, r4
		vp_ret

	fn_function_end
