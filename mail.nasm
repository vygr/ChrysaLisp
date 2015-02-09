%include "mail.inc"

;;;;;;;;;;;
; mail code
;;;;;;;;;;;

	SECTION .text

ml_alloc_mail:
	;outputs
	;r0 = mail message
	;trashes
	;r1-r3

	vp_cpy ml_mail_heap, r0
	vp_call hp_alloc_cell
	vp_cpy r1, r0
	vp_ret
		
ml_free_mail:
	;inputs
	;r1 = mail message
	;trashes
	;r0-r2

	vp_cpy ml_mail_heap, r0
	hp_free_cell r0, r1, r2
	vp_ret

ml_send_mail:
	;inputs
	;r0 = mail message
	;trashes
	;r0-r2

	vp_cpy [r0 + ML_MSG_DEST], r1
	if r1, ==, 0
		;mail for kernel !
		vp_cpy ml_kernel_mailbox, r1
		vp_cpy [r1], r1
	endif
	lh_add_at_head r1, r0, r2
	vp_cpy [r1 + ML_MAILBOX_TCB], r0
	if r0, !=, 0
		vp_cpy 0, long[r1 + ML_MAILBOX_TCB]
		vp_call tk_resume_task
	endif
	vp_ret

ml_receive_mail:
	;inputs
	;r0 = mailbox address
	;outputs
	;r0 = mailbox address
	;r1 = mail address
	;trashes
	;r0-r2

	lh_is_empty r0, r1
	if r1, ==, 0
		vp_cpy r15, [r0 + ML_MAILBOX_TCB]
		vp_call tk_suspend_task
	endif
	lh_remove_tail r0, r1, r2
	vp_ret

;;;;;;;;;;;
; mail data
;;;;;;;;;;;

	SECTION .data

	hp_heap_object ml_mail_heap
ml_kernel_mailbox:
	dq	0
