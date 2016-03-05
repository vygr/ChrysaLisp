%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/task_open_remote, no_debug_enter
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
		ml_temp_create r0, r1

		;allocate mail message
		class_call mail, alloc
		vp_cpy r0, r3

		;fill in destination, reply and function
		fn_call sys/cpu_get_id
		vp_cpy r4, [r3 + (ml_msg_data + kn_data_kernel_reply)]
		vp_cpy r0, [r3 + (ml_msg_data + kn_data_kernel_reply + 8)]
		vp_cpy 0, qword[r3 + ml_msg_dest]
		vp_cpy r6, [r3 + (ml_msg_dest + 8)]
		vp_cpy fn_call_task_child, qword[r3 + (ml_msg_data + kn_data_kernel_function)]

		;copy task name
		vp_cpy r5, r0
		vp_lea [r3 + (ml_msg_data + kn_data_task_child_pathname)], r1
		fn_call sys/string_copy

		;fill in total message length
		vp_sub r3, r1
		vp_cpy r1, [r3 + ml_msg_length]

		;send mail to kernel then wait for reply
		vp_cpy r3, r0
		class_call mail, send
		vp_cpy r4, r0
		class_call mail, read

		;save reply mailbox ID
		vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_mailboxid)], r3
		vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_mailboxid + 8)], r5

		;free reply mail and temp mailbox
		fn_call sys/mem_free
		ml_temp_destroy

		;return mailbox ID
		vp_cpy r3, r0
		vp_cpy r5, r1
		vp_ret

	fn_function_end
