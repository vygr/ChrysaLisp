%include 'inc/func.inc'
%include 'inc/mail.inc'

	fn_function sys/task_callback, no_debug_enter
		;inputs
		;r0 = callback address
		;r1 = user data address
		;trashes
		;all but r4

		;test if we are the kernel task
		static_bind sys_task, statics, r3
		vp_cpy [r3 + tk_statics_current_tcb], r2
		if r2, ==, [r3 + tk_statics_kernel_tcb]
			;yes we can just do local call
			vp_xchg r0, r1
			vp_jmp r1
		endif

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
		vp_cpy r4, [r3 + kn_data_kernel_reply]
		vp_cpy r0, [r3 + kn_data_kernel_reply + 8]
		vp_cpy_cl 0, [r3 + ml_msg_dest]
		vp_cpy r0, [r3 + ml_msg_dest + 8]
		vp_cpy_cl kn_call_callback, [r3 + kn_data_kernel_function]
		vp_cpy r5, [r3 + kn_data_callback_addr]
		vp_cpy r6, [r3 + kn_data_kernel_user]
		vp_cpy_cl kn_data_callback_size, [r3 + ml_msg_length]

		;send mail to kernel then wait for reply
		vp_cpy r3, r0
		static_call sys_mail, send
		vp_cpy r4, r0
		static_call sys_mail, read

		;free reply mail and temp mailbox
		ml_temp_destroy
		static_jmp sys_mem, free

	fn_function_end
