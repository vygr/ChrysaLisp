%include 'inc/func.inc'
%include 'inc/mail.inc'

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
			class_call mail, alloc
			vp_cpy r0, r3

			;fill in destination, reply, function and user
			fn_call sys/cpu_get_id
			vp_cpy 0, qword[r3 + ml_msg_dest]
			vp_cpy r0, [r3 + (ml_msg_dest + 8)]
			vp_cpy r4, [r3 + (ml_msg_data + kn_data_kernel_reply)]
			vp_cpy r0, [r3 + (ml_msg_data + kn_data_kernel_reply + 8)]
			vp_cpy r6, [r3 + (ml_msg_data + kn_data_kernel_user)]
			vp_cpy fn_call_task_child, qword[r3 + (ml_msg_data + kn_data_kernel_function)]

			;copy task name, move to next task name
			vp_cpy r5, r0
			vp_lea [r3 + (ml_msg_data + kn_data_task_child_pathname)], r1
			fn_call sys/string_copy
			vp_cpy r0, r5

			;fill in total message length
			vp_sub r3, r1
			vp_cpy r1, [r3 + ml_msg_length]

			;send mail to kernel
			vp_cpy r3, r0
			class_call mail, send

			;next array worker
			vp_add 16, r6
			vp_inc r7
			vp_cpy byte[r5], r0l
			vp_and 0xff, r0
		loop_until r0, ==, 0

		;wait for all replies
		loop_start
			vp_cpy r4, r0
			class_call mail, read

			;save reply mailbox ID in user address
			vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_user)], r6
			vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_mailboxid)], r2
			vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_mailboxid + 8)], r3
			vp_cpy r2, [r6]
			vp_cpy r3, [r6 + 8]

			;free reply mail
			class_call mem, free

			;next mailbox
			vp_dec r7
		loop_until r7, ==, 0

		;free temp mailbox
		ml_temp_destroy
		vp_ret

	fn_function_end
