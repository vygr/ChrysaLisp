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
%include "syscall.inc"

;;;;;;;;;;;;;
; entry point
;;;;;;;;;;;;;

	SECTION .text

	global _main
_main:
	;init loader
	vp_call ld_init_loader

	;init tasker
	vp_call tk_init_tasker

	;init mailer
	vp_call ld_mail_init_mailer + 0x38

	;start kernel task and save mailbox for others
	vp_call tk_start_task
	vp_cpy r1, r15
	vp_cpy r0, [rel ml_kernel_mailbox]

	;load and run boot task
	vp_cpy boot_task, r0
	vp_call ld_load_function
	vp_call tk_start_task

;;;;;;;;;;;;;;;;;;;;;;;
; main kernal task loop
;;;;;;;;;;;;;;;;;;;;;;;

	;loop till no other tasks running
	repeat
		;allow all other tasks to run
		vp_call tk_deshedule_task

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

		;check if no other tasks
		vp_cpy tk_task_suspend_list, r0
		lh_is_empty r0, r0
		continueif r0, !=, 0
		vp_cpy tk_task_list, r0
		lh_get_head r0, r1
		lh_get_tail r0, r0
	until r1, ==, r0

	;deinit mailer
	vp_call ld_mail_deinit_mailer + 0x38

	;deinit tasker
	vp_call tk_deinit_tasker

	;deinit loader
	vp_call ld_deinit_loader

	;exit !
	sys_exit 0

%include "task.nasm"
%include "load.nasm"

;;;;;;;;;;;;;
; kernel data
;;;;;;;;;;;;;

	SECTION	.data

	align 8, db 0
kernel_table:
	dq	tk_start_task
	dq	tk_stop_task
	dq	tk_suspend_task
	dq	tk_resume_task
	dq	tk_deshedule_task
	dq	ld_load_function

ml_kernel_mailbox:
	dq	0
boot_task:
	db	"sys/boot", 0
