;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nasm -f macho64 main.nasm
;; ld -macosx_version_min 10.6 -o main -e _main main.o
;; ./main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "vp.nasm"
%include "code.nasm"
%include "list.nasm"
%include "heap.nasm"
%include "mail.nasm"
%include "task.nasm"

%include "syscall.nasm"
%include "util.nasm"

;;;;;;;;;;;;;
; entry point
;;;;;;;;;;;;;

	SECTION .text

	global _main
_main:
	;init task control block heap
	vp_cpy tk_task_heap, r0
	vp_cpy TK_NODE_SIZE, r1
	vp_cpy TK_NODE_SIZE*8, r2
	vp_call hp_init

	;init mail message heap
	vp_cpy ml_mail_heap, r0
	vp_cpy ML_MSG_SIZE, r1
	vp_cpy ML_MSG_SIZE*256, r2
	vp_call hp_init

	;init task lists
	vp_cpy tk_task_list, r0
	lh_init r0, r1
	vp_cpy tk_task_suspend_list, r0
	lh_init r0, r1

	;start kernel task and save mailbox for others
	vp_call tk_start_task
	vp_cpy r1, r15
	vp_cpy ml_kernel_mailbox, r1
	vp_cpy r0, [r1]

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	;start task one, save mailbox
	vp_cpy task_one_entry, r0
	vp_call tk_start_task
	vp_cpy r0, r14

	;alloc mail message, send to task one
	vp_call ml_alloc_mail
	vp_cpy r14, [r0 + ML_MSG_DEST]
	vp_call ml_send_mail

;;;;;;;;;;;;;;;;;;;;;;;
; main kernal task loop
;;;;;;;;;;;;;;;;;;;;;;;

	;loop till no other tasks running
	repeat
		;allow all other tasks to run
		vp_call tk_deshedule

		;service all kernel mail
		loopstart
			;check if any mail
			vp_lea [r15 + TK_NODE_MAILBOX], r0
			ml_check r0, r1
			breakif r1, e, 0

			;handle kernel request and reply
			vp_call ml_receive_mail
			vp_cpy r1, r0
			vp_cpy [r0 + (ML_MSG_DATA + ML_DATA_KERNEL_REPLY)], r1
			vp_cpy [r0 + (ML_MSG_DATA + ML_DATA_KERNEL_REPLY + 8)], r2
			vp_cpy r1, [r0 + ML_MSG_DEST]
			vp_cpy r2, [r0 + (ML_MSG_DEST + 8)]
			vp_cpy [r0 + (ML_MSG_DATA + ML_DATA_KERNEL_FUNC)], r1
			switch
			case r1, e, 0
				break
			case r1, e, 1
				break
			default
			endswitch
			vp_call ml_send_mail
		loopend

		;check if no other tasks
		vp_cpy tk_task_suspend_list, r0
		lh_is_empty r0, r0
		continueif r0, ne, 0
		vp_cpy tk_task_list, r0
		lh_get_head r0, r1
		lh_get_tail r0, r0
	until r1, e, r0

	;free the task heap
	vp_cpy tk_task_heap, r0
	vp_call hp_free_heap
	vp_call hp_deinit

	;exit !
	sys_exit 0

;;;;;;;;;;;
; test code
;;;;;;;;;;;

;;;;;;;;;;
; task one
;;;;;;;;;;

task_one_entry:
	;read my mail for go message, then free
	vp_lea [r15 + TK_NODE_MAILBOX], r0
	vp_call ml_receive_mail
	vp_call ml_free_mail

	;start task two
	vp_cpy task_two_entry, r0
	vp_call tk_start_task

	for r11, 0, 10, 1
		;print lower case ascii table
		for r8, 0, 10, 1
			for r9, 0, 10, 1
				vp_cpy r8, r10
				vp_add r9,r10
				vp_add 'a', r10
				sys_write_char 1, r10
				sys_write_char 1, ' '
			next
			sys_write_char 1, 10
		next
		sys_write_char 1, 10

		vp_call tk_deshedule
	next
	vp_call tk_stop_task

;;;;;;;;;;
; task two
;;;;;;;;;;

task_two_entry:
	;start task three
	vp_cpy task_three_entry, r0
	vp_call tk_start_task
	vp_cpy r0, r14

	;alloc 1000000 mail messages, send to task four
	for r8, 0, 1000000, 1
		vp_call ml_alloc_mail
		vp_cpy r14, [r0 + ML_MSG_DEST]
		vp_call ml_send_mail

		vp_call tk_deshedule
	next

	for r11, 0, 10, 1
		;print lower case ascii table
		for r8, 0, 10, 1
			for r9, 0, 10, 1
				vp_cpy r8, r10
				vp_add r9,r10
				vp_add 'A', r10
				sys_write_char 1, r10
				sys_write_char 1, ' '
			next
			sys_write_char 1, 10
		next
		sys_write_char 1, 10

		vp_call tk_deshedule
	next
	vp_call tk_stop_task

;;;;;;;;;;;;
; task three
;;;;;;;;;;;;

task_three_entry:
	;say hello
	vp_cpy hi, r0
	sys_write_string 1, r0, hie-hi

	;read my mail for 1000000 messages
	for r8, 0, 1000000, 1
		vp_lea [r15 + TK_NODE_MAILBOX], r0
		vp_call ml_receive_mail
		vp_call ml_free_mail
	next

	;wave goodbye
	vp_cpy bi, r0
	sys_write_string 1, r0, bie-bi
	vp_call tk_stop_task

;;;;;;;;;;;
; test data
;;;;;;;;;;;

	SECTION	.data
hi:
	db	"Hello from task three !", 10, 10
hie:
bi:
	db	"Goodbye from task three !", 10, 10
bie:
