%include "func.inc"
%include "mail.inc"

	fn_function "sys/task_open_remote"
		;inputs
		;r0 = new task function name
		;r1 = cpu id to start with
		;outputs
		;r0, r1 = new task mailbox ID
		;trashes
		;r2-r3, r5-r6

		;save task info
		vp_cpy r0, r5
		vp_cpy r1, r6

		;create temp mailbox
		ml_temp_create r0

		;allocate mail message
		fn_call sys/mail_alloc
		vp_cpy r0, r3

		;fill in destination, reply and function
		fn_call sys/get_cpu_id
		vp_cpy r4, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY)]
		vp_cpy r0, [r3 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY + 8)]
		vp_cpy 0, qword[r3 + ML_MSG_DEST]
		vp_cpy r6, [r3 + (ML_MSG_DEST + 8)]
		vp_cpy KN_CALL_TASK_CHILD, qword[r3 + (ML_MSG_DATA + KN_DATA_KERNEL_FUNCTION)]

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
		vp_cpy r4, r0
		fn_call sys/mail_read

		;save reply mailbox ID
		vp_cpy [r1 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_MAILBOXID)], r3
		vp_cpy [r1 + (ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_MAILBOXID + 8)], r5

		;free reply mail and temp mailbox
		vp_cpy r1, r0
		fn_call sys/mem_free
		ml_temp_destroy

		;return mailbox ID
		vp_cpy r3, r0
		vp_cpy r5, r1
		vp_ret

	fn_function_end
