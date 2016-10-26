%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/task.inc'
%include 'inc/link.inc'
%include 'inc/string.inc'
%include 'inc/load.inc'

	def_func sys/opt_process
		;process command options
		;inputs
		;r0 = argv array
		;trashes
		;r0-r3, r5-r14

		vp_cpy r0, r14
		loop_start
			vp_cpy [r14], r13
			breakif r13, ==, 0
			vp_rel options_table, r12
			loop_start
				vp_cpy [r12], r11
				breakif r11, ==, 0
				vp_add 8, r12
				f_call sys_string, compare, {r12, r13}, {r0}
				if r0, ==, 0
					vp_rel options_table, r0
					vp_add r11, r0
					vp_call r0
					vp_jmp next_arg
				endif
				f_call sys_string, length, {r12}, {r1}
				vp_add r1, r12
				vp_add ptr_size, r12
				vp_and -ptr_size, r12
			loop_end
		next_arg:
			vp_cpy [r14], r0
			breakif r0, ==, 0
			vp_add 8, r14
		loop_end
		vp_ret

	opt_cpu:
		;inputs
		;r14 = arg pointer
		;outputs
		;r14 = arg pointer updated

		;set cpu ID
		vp_add 8, r14
		vp_cpy [r14], r0
		if r0, !=, 0
			f_call sys_string, to_long, {r0, 10}, {r0}
			f_bind sys_task, statics, r1
			vp_cpy r0, [r1 + tk_statics_cpu_id]
		endif
		vp_ret

	opt_run:
		;inputs
		;r14 = arg pointer
		;outputs
		;r14 = arg pointer updated

		;load and run task
		vp_add 8, r14
		vp_cpy [r14], r0
		if r0, !=, 0
			f_call sys_load, bind, {r0}, {r0}
			if r0, !=, 0
				f_call sys_task, start, {r0}, {r0, r1}
			endif
		endif
		vp_ret

	opt_link:
		;inputs
		;r14 = arg pointer
		;outputs
		;r14 = arg pointer updated

		;start link task
		vp_add 8, r14
		vp_cpy [r14], r0
		if r0, !=, 0
			;start link
			func_path sys_link, link
			f_call sys_task, start, {@_function_}, {r0, r5}
			f_call sys_cpu, id, {}, {r6}

			;allocate params message
			f_call sys_mail, alloc, {}, {r7}
			assert r0, !=, 0

			;fill in destination
			vp_cpy r5, [r0 + msg_dest]
			vp_cpy r6, [r0 + (msg_dest + 8)]

			;fill in paramaters and set length
			f_call sys_string, copy, {$link_path, &[r7 + msg_data]}, {_, r1}
			vp_dec r1
			f_call sys_string, copy, {[r14], r1}, {_, r1}
			vp_sub r7, r1
			vp_cpy r1, [r7 + msg_length]

			;send to link task
			f_call sys_mail, send, {r7}
		endif
		vp_ret

	link_path:
		db '/tmp/', 0

		align 8, db 0
	options_table:
		dq	opt_cpu - options_table
			db	'-cpu', 0
			align 8, db 0
		dq	opt_run - options_table
			db	'-run', 0
			align 8, db 0
		dq	opt_link - options_table
			db	'-l', 0
			align 8, db 0
		dq	0

	def_func_end
