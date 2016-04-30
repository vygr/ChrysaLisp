%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/string.inc'

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
		static_call sys_mail, alloc, {}, {r3}
		assert r0, !=, 0

		;fill in destination, reply and function
		static_call sys_cpu, id, {}, {r0}
		vp_cpy r4, [r3 + kn_data_kernel_reply]
		vp_cpy r0, [r3 + kn_data_kernel_reply + 8]
		vp_cpy_cl 0, [r3 + ml_msg_dest]
		vp_cpy r6, [r3 + (ml_msg_dest + 8)]
		vp_cpy_cl kn_call_task_child, [r3 + kn_data_kernel_function]

		;copy task name
		static_call sys_string, copy, {r5, :[r3 + kn_data_task_child_pathname]}, {_, r1}

		;fill in total message length
		vp_sub r3, r1
		vp_cpy r1, [r3 + ml_msg_length]

		;send mail to kernel then wait for reply
		static_call sys_mail, send, {r3}
		static_call sys_mail, read, {r4}, {r0}

		;save reply mailbox ID
		vp_cpy [r0 + kn_data_task_child_reply_mailboxid], r3
		vp_cpy [r0 + kn_data_task_child_reply_mailboxid + 8], r5

		;free reply mail and temp mailbox
		static_call sys_mem, free, {r0}
		ml_temp_destroy

		;return mailbox ID
		vp_cpy r3, r0
		vp_cpy r5, r1
		vp_ret

	fn_function_end
