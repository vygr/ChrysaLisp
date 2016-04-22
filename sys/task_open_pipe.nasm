%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/string.inc'

	fn_function sys/task_open_pipe, no_debug_enter
		;inputs
		;r0 = new task function names
		;r1 = mailbox array pointer
		;trashes
		;r0-r3, r5-r7

		;save task info
		vp_cpy r0, r5
		vp_cpy r1, r6

		;create temp mailbox
		ml_temp_create r0, r1

		;start all tasks, starting with kernel of this chip
		static_call sys_cpu, id
		vp_cpy r0, r7
		loop_start
			;allocate mail message
			static_call sys_mail, alloc
			assert r0, !=, 0
			vp_cpy r0, r3

			;fill in destination, reply, function
			static_call sys_cpu, id
			vp_cpy_cl 0, [r3 + ml_msg_dest]
			vp_cpy r7, [r3 + ml_msg_dest + 8]
			vp_cpy r4, [r3 + kn_data_kernel_reply]
			vp_cpy r0, [r3 + kn_data_kernel_reply + 8]
			vp_cpy_cl kn_call_task_child, [r3 + kn_data_kernel_function]

			;copy task name, move to next task name
			vp_cpy r5, r0
			vp_lea [r3 + kn_data_task_child_pathname], r1
			static_call sys_string, copy
			vp_cpy r0, r5

			;fill in total message length
			vp_sub r3, r1
			vp_cpy r1, [r3 + ml_msg_length]

			;send mail to a kernel, wait for reply
			vp_cpy r3, r0
			static_call sys_mail, send
			vp_cpy r4, r0
			static_call sys_mail, read

			;save reply mailbox ID
			vp_cpy [r0 + kn_data_task_child_reply_mailboxid], r2
			vp_cpy [r0 + kn_data_task_child_reply_mailboxid + 8], r3
			vp_cpy r2, [r6]
			vp_cpy r3, [r6 + 8]
			vp_cpy r3, r7	;near this cpu next

			;free reply mail
			static_call sys_mem, free

			;next pipe worker
			vp_add mailbox_id_size, r6
			vp_xor r0, r0
			vp_cpy_b [r5], r0
		loop_until r0, ==, 0

		;free temp mailbox
		ml_temp_destroy
		vp_ret

	fn_function_end
