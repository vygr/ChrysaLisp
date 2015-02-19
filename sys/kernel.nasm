%include "func.inc"
%include "mail.inc"
%include "task.inc"

;;;;;;;;;;;;;
; kernel task
;;;;;;;;;;;;;

	fn_function "sys/kernel"
		;loader is allready initialized when we get here !

		;init tasker
		fn_call sys/task_init_tasker

		;init mailer
		fn_call sys/mail_init_mailer

		;init allocator
		fn_call sys/mem_init_allocator

		;start kernel task and patch mailbox
		fn_call sys/task_start
		vp_cpy r1, r15
		fn_bind sys/mail_send, r1
		vp_cpy r0, [r1 + 0x40]

		;process command options
		vp_add 8, r4
		vp_cpy r4, r14
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

;;;;;;;;;;;;;;;;;;;;;;;
; main kernal task loop
;;;;;;;;;;;;;;;;;;;;;;;

		;loop till no other tasks running
		repeat
			;allow all other tasks to run
			fn_call sys/task_deshedule

			;service all kernel mail
			loopstart
				;check if any mail
				vp_lea [r15 + TK_NODE_MAILBOX], r0
				ml_check r0, r1
				breakif r1, ==, 0

				;read waiting mail
				fn_call sys/mail_read
				vp_cpy r1, r14

				;fill in reply ID, user field is left alone !
				vp_cpy [r14 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY)], r1
				vp_cpy [r14 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY + 8)], r2
				vp_cpy r1, [r14 + ML_MSG_DEST]
				vp_cpy r2, [r14 + (ML_MSG_DEST + 8)]

				;switch on kernel call number
				vp_cpy [r14 + (ML_MSG_DATA + KN_DATA_KERNEL_FUNCTION)], r1
				switch
				case r1, ==, KN_CALL_TASK_OPEN
					;open single task and return mailbox ID
					vp_lea [r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_PATHNAME)], r0
					fn_call sys/load_function_load
					fn_call sys/task_start
					vp_cpy r0, [r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_REPLY_MAILBOXID)]
					vp_cpy 0, long[r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_REPLY_MAILBOXID + 8)]
					vp_cpy ML_MSG_DATA + KN_DATA_TASK_OPEN_REPLY_SIZE, long[r14 + ML_MSG_LENGTH]
					break
				case r1, ==, KN_CALL_TASK_CHILD
					;distribute single task and return mailbox ID
					vp_lea [r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_PATHNAME)], r0
					fn_call sys/load_function_load
					fn_call sys/task_start
					vp_cpy r0, [r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_REPLY_MAILBOXID)]
					vp_cpy 0, long[r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_REPLY_MAILBOXID + 8)]
					vp_cpy ML_MSG_DATA + KN_DATA_TASK_CHILD_REPLY_SIZE, long[r14 + ML_MSG_LENGTH]
					break
				default
				endswitch
				vp_cpy r14, r0
				fn_call sys/mail_send
			loopend

			;start any tasks ready to restart
			fn_bind sys/task_statics, r3
			vp_lea [r3 + TK_STATICS_TASK_TIMER_LIST], r0
			lh_is_empty r0, r0
			if r0, !=, 0
				;get time
				vp_sub TIMEVAL_SIZE, r4
				vp_cpy r4, r0
				sys_gettimeofday r0, 0
				vp_mul 1000000, r0
				vp_add r0, r2
				vp_add TIMEVAL_SIZE, r4

				vp_cpy [r3 + TK_STATICS_TASK_TIMER_LIST + LH_LIST_HEAD], r0
				loopstart
					vp_cpy r0, r1
					ln_get_succ r0, r0
					breakif r0, ==, 0
					vp_cpy [r1 + TK_NODE_TIME], r5
					if r5, <=, r2
						;task ready, remove from timer list and place on ready list
						vp_cpy r1, r5
						ln_remove_node r5, r6
						vp_lea [r3 + TK_STATICS_TASK_LIST], r5
						lh_add_at_head r5, r1, r6
					endif
				loopend
			endif

			;check if no other tasks available
			vp_lea [r3 + TK_STATICS_TASK_TIMER_LIST], r0
			lh_is_empty r0, r0
			continueif r0, !=, 0
			vp_lea [r3 + TK_STATICS_TASK_SUSPEND_LIST], r0
			lh_is_empty r0, r0
			continueif r0, !=, 0
			vp_lea [r3 + TK_STATICS_TASK_LIST], r0
			lh_get_head r0, r1
			lh_get_tail r0, r0
		until r1, ==, r0

		;deinit allocator
		fn_call sys/mem_deinit_allocator

		;deinit mailer
		fn_call sys/mail_deinit_mailer

		;deinit tasker
		fn_call sys/task_deinit_tasker

		;deinit loader
		fn_call sys/load_deinit_loader

		;exit !
		sys_exit 0

;;;;;;;;;;;;;;;;;;;;;;;;;;
; kernel option processors
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
			fn_call sys/load_function_load
			if r0, !=, 0
				fn_call sys/task_start
			endif
		endif
		vp_ret

;;;;;;;;;;;;;
; kernel data
;;;;;;;;;;;;;

		align 8, db 0
	options_table:
		dq	opt_cpu - options_table
			db	"-cpu", 0
			align 8, db 0
		dq	opt_run - options_table
			db	"-run", 0
			align 8, db 0
		dq	0

	fn_function_end
