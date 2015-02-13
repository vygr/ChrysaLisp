;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nasm -f macho64 main.nasm
;; ld -macosx_version_min 10.6 -o main -e _main main.o
;; ./main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "vp.inc"
%include "code.inc"
%include "list.inc"
%include "mail.inc"
%include "task.inc"
%include "load.inc"
%include "syscall.inc"

;;;;;;;;;;;;;
; entry point
;;;;;;;;;;;;;

	SECTION .text

	global _main
_main:
	;set prebound functions as executable
	vp_lea [rel ld_prebound], r0
	vp_and -LD_PAGE_SIZE, r0
	vp_lea [rel ld_prebounde], r1
	vp_sub r0, r1
	sys_mprotect r0, r1, PROT_READ|PROT_WRITE|PROT_EXEC

	;init loader
	vp_call ld_load_init_loader + 0x38

	;init tasker
	vp_call ld_task_init_tasker + 0x38

	;init mailer
	vp_call ld_mail_init_mailer + 0x38

	;init allocator
	vp_call ld_mem_init_allocator + 0x38

	;start kernel task and patch mailbox
	vp_call ld_task_start + 0x30
	vp_cpy r1, r15
	vp_cpy r0, [rel ld_mail_send + 0x70]

	;load and run boot task
	vp_cpy boot_task, r0
	vp_call ld_load_function_load + 0x38
	vp_call ld_task_start + 0x30

;;;;;;;;;;;;;;;;;;;;;;;
; main kernal task loop
;;;;;;;;;;;;;;;;;;;;;;;

	;loop till no other tasks running
	repeat
		;allow all other tasks to run
		vp_call ld_task_deshedule + 0x38

		;service all kernel mail
		loopstart
			;check if any mail
			vp_lea [r15 + TK_NODE_MAILBOX], r0
			ml_check r0, r1
			breakif r1, ==, 0

			;handle kernel request and reply
			vp_call ld_mail_read + 0x30
			vp_cpy r1, r0
			vp_cpy [r0 + (ML_MSG_DATA + ML_DATA_KERNEL_REPLY)], r1
			vp_cpy [r0 + (ML_MSG_DATA + ML_DATA_KERNEL_REPLY + 8)], r2
			vp_cpy r1, [r0 + ML_MSG_DEST]
			vp_cpy r2, [r0 + (ML_MSG_DEST + 8)]
			vp_cpy [r0 + (ML_MSG_DATA + ML_DATA_KERNEL_FUNC)], r1
			switch
			case r1, ==, 0
				break
			case r1, ==, 1
				break
			default
			endswitch
			vp_call ld_mail_send + 0x30
		loopend

		;check if any timer delayed tasks
		vp_call ld_task_get_statics + 0x38
		vp_cpy r0, r2
		vp_lea [r2 + TK_STATICS_TASK_TIMER_LIST], r0
		lh_is_empty r0, r0
		if r0, !=, 0
			;get time
			vp_sub TIMEVAL_SIZE, r4
			vp_xor r0, r0
			vp_cpy r0, [r4 + TIMEVAL_SEC]
			vp_cpy r0, [r4 + TIMEVAL_USEC]
			vp_cpy r4, r0
			sys_gettimeofday r0, 0
			vp_cpy [r4 + TIMEVAL_SEC], r3
			vp_mul 1000000, r3
			vp_add [r4 + TIMEVAL_USEC], r3
			vp_add TIMEVAL_SIZE, r4

			;start any tasks ready
			vp_cpy [r2 + TK_STATICS_TASK_TIMER_LIST + LH_LIST_HEAD], r0
			loopstart
				vp_cpy r0, r1
				ln_get_succ r0, r0
				breakif r0, ==, 0
				vp_cpy [r1 + TK_NODE_TIME], r5
				if r5, <=, r3
					;task ready, remove from timer list and place on ready list
					vp_cpy r1, r5
					ln_remove_node r5, r6
					vp_lea [r2 + TK_STATICS_TASK_LIST], r5
					lh_add_at_head r5, r1, r6
				endif
			loopend
		endif

		;check if no other tasks available
		vp_lea [r2 + TK_STATICS_TASK_TIMER_LIST], r0
		lh_is_empty r0, r0
		continueif r0, !=, 0
		vp_lea [r2 + TK_STATICS_TASK_SUSPEND_LIST], r0
		lh_is_empty r0, r0
		continueif r0, !=, 0
		vp_lea [r2 + TK_STATICS_TASK_LIST], r0
		lh_get_head r0, r1
		lh_get_tail r0, r0
	until r1, ==, r0

	;deinit allocator
	vp_call ld_mem_deinit_allocator + 0x38

	;deinit mailer
	vp_call ld_mail_deinit_mailer + 0x38

	;deinit tasker
	vp_call ld_task_deinit_tasker + 0x38

	;deinit loader
	vp_call ld_load_deinit_loader + 0x38

	;exit !
	sys_exit 0

;;;;;;;;;;;;;
; kernel data
;;;;;;;;;;;;;

	SECTION	.data

boot_task:
	db	"sys/boot", 0
	align 8, db 0

;;;;;;;;;;;;;;;;;;;;;;;;
; prebound function data
;;;;;;;;;;;;;;;;;;;;;;;;

	align 8, db 0
ld_prebound:

ld_load_init_loader:
	incbin	'sys/load_init_loader'		;must be first function !
ld_load_function_load:
	incbin	'sys/load_function_load'	;must be second function !
	incbin	'sys/load_get_statics'		;must be third function !
ld_load_deinit_loader:
	incbin	'sys/load_deinit_loader'

ld_mail_init_mailer:
	incbin	'sys/mail_init_mailer'
ld_mail_deinit_mailer:
	incbin	'sys/mail_deinit_mailer'
ld_mail_send:
	incbin	'sys/mail_send'
ld_mail_read:
	incbin	'sys/mail_read'

ld_task_get_statics:
	incbin	'sys/task_get_statics'
ld_task_init_tasker:
	incbin	'sys/task_init_tasker'
ld_task_deinit_tasker:
	incbin	'sys/task_deinit_tasker'
ld_task_deshedule:
	incbin	'sys/task_deshedule'
ld_task_start:
	incbin	'sys/task_start'

ld_mem_init_allocator:
	incbin	'sys/mem_init_allocator'
ld_mem_deinit_allocator:
	incbin	'sys/mem_deinit_allocator'

ld_prebounde:
	dq 0
