%ifndef TASK_1234
	%define TASK_1234

%include 'inc/list.inc'
%include 'inc/mail.inc'
%include 'inc/heap.inc'

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

	struc TK_NODE
		TK_NODE_NODE:		resb	ln_node_size
		TK_NODE_STACK:		resq	1
		TK_NODE_TIME:		resq	1
		TK_NODE_MAILBOX:	resb	ML_MAILBOX_SIZE
		TK_NODE_STACKSPACE:	resb	1024*4
		TK_NODE_SIZE:
	endstruc

	struc TK_STATICS
		TK_STATICS_CURRENT_TCB:	resq	1
		TK_STATICS_TASK_HEAP:	resb	hp_heap_size
		TK_STATICS_TASK_LIST:	resb	lh_list_size
		TK_STATICS_TIMER_LIST:	resb	lh_list_size
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
		vp_push r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14
	%endmacro

	%macro tk_load_state 0
		vp_pop r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14
	%endmacro

%endif
