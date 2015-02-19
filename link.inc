%ifndef LINK_1234
    %define LINK_1234

%include "list.inc"
%include "mail.inc"

;;;;;;;;;;;;;;;;;
; link structures
;;;;;;;;;;;;;;;;;

	struc LK_NODE
		LK_NODE_NODE:			resb	LN_NODE_SIZE
		LK_NODE_CPU_ID:			resq	1
		LK_NODE_TASK_COUNT:		resq	1
		LK_NODE_SIZE:
	endstruc

	struc LK_STATICS
		LK_STATICS_LINKS_LIST:	resb	LH_LIST_SIZE
		LK_STATICS_SIZE:
	endstruc

	struc LK_BUFFER
		LK_BUFFER_OUT_STATUS:		resq	1
		LK_BUFFER_OUT_TASK_COUNT:	resq	1
		LK_BUFFER_OUT_MSG:			resb	ML_MSG_SIZE
		LK_BUFFER_IN_STATUS:		resq	1
		LK_BUFFER_IN_TASK_COUNT:	resq	1
		LK_BUFFER_IN_MSG:			resb	ML_MSG_SIZE
		LK_BUFFER_SIZE:
	endstruc

%endif
