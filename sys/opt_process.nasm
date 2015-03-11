%include "func.inc"
%include "mail.inc"
%include "task.inc"
%include "link.inc"

	fn_function "sys/opt_process"
		;process command options
		;inputs
		;r0 = argv array
		;trashes
		;r0-r3, r5-r14

		vp_cpy r0, r14
		loopstart
			vp_cpy [r14], r13
			breakif r13, ==, 0
			vp_lea [rel options_table], r12
			loopstart
				vp_cpy [r12], r11
				breakif r11, ==, 0
				vp_add 8, r12
				vp_cpy r12, r0
				vp_cpy r13, r1
				fn_call sys/string_compare
				if r0, !=, 0
					vp_lea [rel options_table], r0
					vp_add r11, r0
					vp_call r0
					vp_jmp next_arg
				endif
				vp_cpy r12, r0
				fn_call sys/string_length
				vp_add r1, r12
				vp_add 8, r12
				vp_and -8, r12
			loopend
		next_arg:
			vp_cpy [r14], r0
			breakif r0, ==, 0
			vp_add 8, r14
		loopend
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
sys_write_char 1, 'C'
			vp_cpy 10, r1
			fn_call sys/string_parse_int
			fn_bind sys/get_cpu_id, r1
			vp_cpy r0, [r1 + 0x8]
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
sys_write_char 1, 'R'
			fn_call sys/load_function_load
			if r0, !=, 0
				fn_call sys/task_start
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
sys_write_char 1, 'L'
			;start link
			fn_bind sys/link, r0
			fn_call sys/task_start
			vp_cpy r0, r5
			fn_call sys/get_cpu_id
			vp_cpy r0, r6

			;allocate params message
			fn_call sys/mail_alloc
			vp_cpy r0, r7

			;fill in destination
			vp_cpy r5, [r0 + ML_MSG_DEST]
			vp_cpy r6, [r0 + (ML_MSG_DEST + 8)]

			;fill in paramaters and set length
			vp_cpy [r14], r0
			vp_lea [r7 + ML_MSG_DATA], r1
			fn_call sys/string_copy
			vp_sub r7, r1
			vp_cpy r1, [r7 + ML_MSG_LENGTH]

			;send to link task
			vp_cpy r7, r0
			fn_call sys/mail_send
		endif
		vp_ret

		align 8, db 0
	options_table:
		dq	opt_cpu - options_table
			db	"-cpu", 0
			align 8, db 0
		dq	opt_run - options_table
			db	"-run", 0
			align 8, db 0
		dq	opt_link - options_table
			db	"-l", 0
			align 8, db 0
		dq	0

	fn_function_end
