%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/string.inc'

	fn_function sys/task_open_device, no_debug_enter
		;inputs
		;r0 = new task function name
		;r1 = cpu id of task
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
		static_call sys_mail, alloc
		fn_assert r0, !=, 0
		vp_cpy r0, r3

		;fill in destination, reply and function
		static_call sys_cpu, id
		vp_cpy r4, [r3 + (ml_msg_data + kn_data_kernel_reply)]
		vp_cpy r0, [r3 + (ml_msg_data + kn_data_kernel_reply + 8)]
		vp_cpy_cl 0, [r3 + ml_msg_dest]
		vp_cpy r6, [r3 + (ml_msg_dest + 8)]
		vp_cpy_cl kn_call_task_open, [r3 + (ml_msg_data + kn_data_kernel_function)]

		;copy task name
		vp_cpy r5, r0
		vp_lea [r3 + (ml_msg_data + kn_data_task_child_pathname)], r1
		static_call sys_string, copy

		;fill in total message length
		vp_sub r3, r1
		vp_cpy r1, [r3 + ml_msg_length]

		;send mail to kernel then wait for reply
		vp_cpy r3, r0
		static_call sys_mail, send
		vp_cpy r4, r0
		static_call sys_mail, read

		;save reply mailbox ID
		vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_mailboxid)], r3
		vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_mailboxid + 8)], r5

		;free reply mail and temp mailbox
		static_call sys_mem, free
		ml_temp_destroy

		;return mailbox ID
		vp_cpy r3, r0
		vp_cpy r5, r1
		vp_ret

	fn_function_end
