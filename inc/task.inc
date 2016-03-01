%ifndef TASK_1234
	%define TASK_1234

%include "inc/list.inc"
%include "inc/mail.inc"
%include "inc/heap.inc"

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

	struc TK_NODE
		TK_NODE_NODE:		resb	LN_NODE_SIZE
		TK_NODE_STACK:		resq	1
		TK_NODE_TIME:		resq	1
		TK_NODE_MAILBOX:	resb	ML_MAILBOX_SIZE
		TK_NODE_STACKSPACE:	resb	1024*4
		TK_NODE_SIZE:
	endstruc

	struc TK_STATICS
		TK_STATICS_CURRENT_TCB:	resq	1
		TK_STATICS_TASK_HEAP:	resb	HP_HEAP_SIZE
		TK_STATICS_TASK_LIST:	resb	LH_LIST_SIZE
		TK_STATICS_TIMER_LIST:	resb	LH_LIST_SIZE
		TK_STATICS_TASK_COUNT:	resq	1
		TK_STATICS_CPU_TOTAL:	resq	1
		TK_STATICS_CPU_ID:		resq	1
		TK_STATICS_SIZE:
	endstruc

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

	TK_STATE_SIZE 	equ 14*8

	%macro tk_save_state 0
		vp_push r0
		vp_push r1
		vp_push r2
		vp_push r3
		vp_push r5
		vp_push r6
		vp_push r7
		vp_push r8
		vp_push r9
		vp_push r10
		vp_push r11
		vp_push r12
		vp_push r13
		vp_push r14
	%endmacro

	%macro tk_load_state 0
		vp_pop r14
		vp_pop r13
		vp_pop r12
		vp_pop r11
		vp_pop r10
		vp_pop r9
		vp_pop r8
		vp_pop r7
		vp_pop r6
		vp_pop r5
		vp_pop r3
		vp_pop r2
		vp_pop r1
		vp_pop r0
	%endmacro

%endif
