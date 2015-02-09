%ifndef MAIL_1234
    %define MAIL_1234

%include "vp.nasm"
%include "code.nasm"
%include "list.nasm"

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

	struc ML_MAILBOX
		ML_MAILBOX_LIST:		resb LH_LIST_SIZE
		ML_MAILBOX_TCB:			resq 1
		ML_MAILBOX_SIZE:
	endstruc

	struc ML_MSG
		ML_MSG_NODE:			resb LN_NODE_SIZE
		ML_MSG_DEST:			resq 2
		ML_MSG_DATA:			resb 256
		ML_MSG_SIZE:
	endstruc

	struc ML_DATA_KERNEL
		ML_DATA_KERNEL_REPLY:	resq 2
		ML_DATA_KERNEL_FUNC:	resq 1
		ML_DATA_KERNEL_SIZE:
	endstruc

;;;;;;;;;;;;;
; mail macros
;;;;;;;;;;;;;

	%macro ml_init 2
		;inputs
		;%1 = mailbox
		;%2 = temp
		;outputs
		;%1 = mailbox
		;trashes
		;%2

		lh_init %1, %2
		vp_cpy 0, long[%1 + ML_MAILBOX_TCB]
	%endmacro

	%macro ml_check 2
		;inputs
		;%1 = mailbox
		;%2 = temp
		;outputs
		;%1 = mailbox
		;%2 = 0 if no mail

		lh_is_empty %1, %2
	%endmacro

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
	if r1, e, 0
		;mail for kernel !
		vp_cpy ml_kernel_mailbox, r1
		vp_cpy [r1], r1
	endif
	lh_add_at_head r1, r0, r2
	vp_cpy [r1 + ML_MAILBOX_TCB], r0
	if r0, ne, 0
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
	if r1, e, 0
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

%endif
