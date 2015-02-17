%include "func.inc"
%include "mail.inc"

	fn_function "sys/task_open_child"
		;inputs
		;r0 = new task function name
		;outputs
		;r0, r1 = new task mailbox ID
		;trashes
		;r2-r3, r5-r6

		;save task name
		vp_cpy r0, r5

		;create temp mailbox
		vp_sub ML_MAILBOX_SIZE, r4
		vp_cpy r4, r6

		;initialise temp mailbox
		vp_cpy 0, long[r6 + ML_MAILBOX_TCB]
		vp_lea [r6 + ML_MAILBOX_LIST], r0
		lh_init r0, r1

		;allocate mail message
		fn_call sys/mail_alloc
		vp_cpy r0, r3

		;fill in destination, reply and function
		vp_cpy r6, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY)]
		vp_cpy 0, long[r3 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY + 8)]
		vp_cpy 0, long[r3 + ML_MSG_DEST]
		vp_cpy 0, long[r3 + (ML_MSG_DEST + 8)]
		vp_cpy KN_CALL_TASK_CHILD, long[r3 + (ML_MSG_DATA + KN_DATA_KERNEL_FUNCTION)]

		;copy task name
		vp_cpy r5, r0
		vp_lea [r3 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_PATHNAME)], r1
		fn_call sys/string_copy
		
		;fill in total message length
		vp_sub r3, r1
		vp_cpy r1, [r3 + ML_MSG_LENGTH]

		;send mail to kernel then wait for reply
		vp_cpy r3, r0
		fn_call sys/mail_send
		vp_cpy r6, r0
		fn_call sys/mail_read

		;save reply mailbox ID
		vp_cpy [r1 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_MAILBOXID)], r5
		vp_cpy [r1 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_MAILBOXID + 8)], r6

		;free reply mail and temp mailbox
		fn_call sys/mail_free
		vp_add ML_MAILBOX_SIZE, r4

		;return mailbox ID
		vp_cpy r5, r0
		vp_cpy r6, r1
		vp_ret

	fn_function_end
