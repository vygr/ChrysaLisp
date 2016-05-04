%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/string.inc'

	fn_function sys/task_open_array, no_debug_enter
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
			static_call sys_mail, alloc, {}, {r3}
			assert r0, !=, 0

			;fill in destination, reply, function and user
			static_call sys_cpu, id, {}, {r0}
			vp_cpy_cl 0, [r3 + ml_msg_dest]
			vp_cpy r0, [r3 + ml_msg_dest + 8]
			vp_cpy r4, [r3 + kn_data_kernel_reply]
			vp_cpy r0, [r3 + kn_data_kernel_reply + 8]
			vp_cpy r6, [r3 + kn_data_kernel_user]
			vp_cpy_cl kn_call_task_child, [r3 + kn_data_kernel_function]

			;copy task name, move to next task name
			static_call sys_string, copy, {r5, :[r3 + kn_data_task_child_pathname]}, {r5, r1}

			;fill in total message length
			vp_sub r3, r1
			vp_cpy r1, [r3 + ml_msg_length]

			;send mail to kernel
			static_call sys_mail, send, {r3}

			;next array worker
			vp_add mailbox_id_size, r6
			vp_inc r7
			vp_xor r0, r0
			vp_cpy_ub [r5], r0
		loop_until r0, ==, 0

		;wait for all replies
		loop_start
			static_call sys_mail, read, {r4}, {r0}

			;save reply mailbox ID in user address
			vp_cpy [r0 + kn_data_task_child_reply_user], r6
			vp_cpy [r0 + kn_data_task_child_reply_mailboxid], r2
			vp_cpy [r0 + kn_data_task_child_reply_mailboxid + 8], r3
			vp_cpy r2, [r6]
			vp_cpy r3, [r6 + 8]

			;free reply mail
			static_call sys_mem, free, {r0}

			;next mailbox
			vp_dec r7
		loop_until r7, ==, 0

		;free temp mailbox
		ml_temp_destroy
		vp_ret

	fn_function_end
