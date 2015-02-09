%ifndef TASK_1234
    %define TASK_1234

%include "list.inc"
%include "mail.inc"

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

	struc TK_NODE
		TK_NODE_NODE:		resb	LN_NODE_SIZE
		TK_NODE_KERNEL:		resq	1
		TK_NODE_STACK:		resq	1
		TK_NODE_MAILBOX:	resb	ML_MAILBOX_SIZE
		TK_NODE_STACKSPACE:	resb	1024*4
		TK_NODE_SIZE:
	endstruc

%endif
