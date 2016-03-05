%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/string.inc'

	fn_function sys/task_open_farm, no_debug_enter
		;inputs
		;r0 = new task function name
		;r1 = mailbox array pointer
		;r2 = farm size, in tasks
		;trashes
		;r0-r3, r5-r8

		;save task info
		vp_cpy r0, r5
		vp_cpy r1, r6
		vp_cpy r2, r7
		vp_cpy r2, r8

		;create temp mailbox
		ml_temp_create r0, r1

		;start all tasks
		loop_start
			;allocate mail message
			class_call mail, alloc
			fn_assert r0, !=, 0
			vp_cpy r0, r3

			;fill in destination, reply and function
			class_call cpu, id
			vp_cpy 0, qword[r3 + ml_msg_dest]
			vp_cpy r0, [r3 + (ml_msg_dest + 8)]
			vp_cpy r4, [r3 + (ml_msg_data + kn_data_kernel_reply)]
			vp_cpy r0, [r3 + (ml_msg_data + kn_data_kernel_reply + 8)]
			vp_cpy kn_call_task_child, qword[r3 + (ml_msg_data + kn_data_kernel_function)]

			;copy task name
			vp_cpy r5, r0
			vp_lea [r3 + (ml_msg_data + kn_data_task_child_pathname)], r1
			class_call string, copy

			;fill in total message length
			vp_sub r3, r1
			vp_cpy r1, [r3 + ml_msg_length]

			;send mail to kernel
			vp_cpy r3, r0
			class_call mail, send

			;next farm worker
			vp_dec r7
		loop_until r7, ==, 0

		;wait for all replies
		loop_start
			vp_cpy r4, r0
			class_call mail, read

			;save reply mailbox ID
			vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_mailboxid)], r2
			vp_cpy [r0 + (ml_msg_data + kn_data_task_child_reply_mailboxid + 8)], r3
			vp_cpy r2, [r6]
			vp_cpy r3, [r6 + 8]

			;free reply mail
			class_call mem, free

			;next mailbox
			vp_add 16, r6
			vp_dec r8
		loop_until r8, ==, 0

		;free temp mailbox
		ml_temp_destroy
		vp_ret

	fn_function_end
