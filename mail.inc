%ifndef MAIL_1234
    %define MAIL_1234

%include "vp.inc"
%include "code.inc"
%include "list.inc"

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
		ML_MSG_LENGTH:			resq 1
		ML_MSG_DEST:			resq 2
		ML_MSG_DATA:			resb 256
		ML_MSG_SIZE:
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

%endif
