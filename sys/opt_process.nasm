%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/task.inc'
%include 'inc/link.inc'
%include 'inc/string.inc'
%include 'inc/load.inc'

	fn_function sys/opt_process, no_debug_enter
		;process command options
		;inputs
		;r0 = argv array
		;trashes
		;r0-r3, r5-r14

		vp_cpy r0, r14
		loop_start
			vp_cpy [r14], r13
			breakif r13, ==, 0
			vp_lea [rel options_table], r12
			loop_start
				vp_cpy [r12], r11
				breakif r11, ==, 0
				vp_add 8, r12
				vp_cpy r12, r0
				vp_cpy r13, r1
				static_call string, compare
				if r0, !=, 0
					vp_lea [rel options_table], r0
					vp_add r11, r0
					vp_call r0
					vp_jmp next_arg
				endif
				vp_cpy r12, r0
				static_call string, length
				vp_add r1, r12
				vp_add 8, r12
				vp_and -8, r12
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
			vp_cpy 10, r1
			static_call string, int
			static_bind task, statics, r1
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
			static_call load, bind
			if r0, !=, 0
				static_call task, start
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
			static_bind link, link, r0
			static_call task, start
			static_call cpu, id
			vp_cpy r1, r5
			vp_cpy r0, r6

			;allocate params message
			static_call mail, alloc
			fn_assert r0, !=, 0
			vp_cpy r0, r7

			;fill in destination
			vp_cpy r5, [r0 + ml_msg_dest]
			vp_cpy r6, [r0 + (ml_msg_dest + 8)]

			;fill in paramaters and set length
			vp_lea [rel link_path], r0
			vp_lea [r7 + ml_msg_data], r1
			static_call string, copy
			vp_cpy [r14], r0
			vp_dec r1
			static_call string, copy
			vp_sub r7, r1
			vp_cpy r1, [r7 + ml_msg_length]

			;send to link task
			vp_cpy r7, r0
			static_call mail, send
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

	fn_function_end
