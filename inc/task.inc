%ifndef TASK_1234
	%define TASK_1234

%include 'inc/list.inc'
%include 'inc/mail.inc'
%include 'inc/heap.inc'

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

	tk_stack_size	equ 1024*4
	struc tk_node
		tk_node_node:		resb	ln_node_size
		tk_node_stack:		resq	1
		tk_node_time:		resq	1
		tk_node_mailbox:	resb	ml_mailbox_size
		tk_node_stackspace:	resb	tk_stack_size
	endstruc

	struc tk_statics
		tk_statics_current_tcb:	resq	1
		tk_statics_task_heap:	resb	hp_heap_size
		tk_statics_task_list:	resb	lh_list_size
		tk_statics_timer_list:	resb	lh_list_size
		tk_statics_task_count:	resq	1
		tk_statics_cpu_total:	resq	1
		tk_statics_cpu_id:		resq	1
	endstruc

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

	tk_state_size 	equ 14*8

	%macro tk_save_state 0
		vp_push r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14
	%endmacro

	%macro tk_load_state 0
		vp_pop r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14
	%endmacro

%endif
