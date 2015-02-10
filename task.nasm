%include "task.inc"

;;;;;;;;;;;
; task code
;;;;;;;;;;;

	SECTION .text

tk_init_tasker:
	;init task control block heap
	vp_cpy tk_task_heap, r0
	vp_cpy TK_NODE_SIZE, r1
	vp_cpy TK_NODE_SIZE*8, r2
	vp_call hp_init

	;init task lists
	vp_cpy tk_task_list, r0
	lh_init r0, r1
	vp_cpy tk_task_suspend_list, r0
	lh_init r0, r1
	vp_ret

tk_deinit_tasker:
	;free the task heap
	vp_cpy tk_task_heap, r0
	vp_call hp_free_heap
	vp_call hp_deinit
	vp_ret

tk_deshedule_task:
	;inputs
	;r15 = task control node

	;push all old task registers
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

	;save old stack pointer
	vp_cpy r4, [r15 + TK_NODE_STACK]

	;get next task control block
	ln_get_succ r15, r15

tk_restore_task:
	;restore next task
	ln_get_succ r15, r0
	if r0, ==, 0
		vp_cpy tk_task_list, r0
		lh_get_head r0, r15
	endif

	;restore old stack pointer
	vp_cpy [r15 + TK_NODE_STACK], r4

	;pop all new registers
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
	vp_ret

tk_start_task:
	;inputs
	;r0 = new task program counter
	;outputs
	;r0 = new task mailbox
	;trashes
	;r0-r5

	;save prog counter
	vp_cpy r0,r5

	;create new task control block and task
	vp_cpy tk_task_heap, r0
	vp_call hp_alloc_cell
	vp_cpy tk_task_list, r0
	lh_add_at_head r0, r1, r2

	;initialise task mailbox
	vp_lea [r1 + TK_NODE_MAILBOX], r0
	ml_init r0, r2

	;fill in kernel table address
	vp_cpy kernel_table, r0
	vp_cpy r0, [r1 + TK_NODE_KERNEL]

	;set task control block stack and return address
	vp_lea [r1 + TK_NODE_SIZE], r0
	vp_sub 8, r0
	vp_cpy r5, [r0]
	vp_sub 14*8, r0
	vp_cpy r0, [r1 + TK_NODE_STACK]

	;return mailbox pointer
	vp_lea [r1 + TK_NODE_MAILBOX], r0
	vp_ret

tk_stop_task:
	;inputs
	;r15 = task control node
	;trashes
	;r0-r15

	;remove task control block
	vp_cpy r15, r0
	vp_cpy r15, r1
	ln_remove_node r0, r15

	;free our task control block
	vp_cpy tk_task_heap, r0
	hp_free_cell r0, r1, r2
	vp_jmp tk_restore_task

tk_suspend_task:
	;inputs
	;r15 = task control node

	;push all task registers
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

	;save stack pointer
	vp_cpy r4, [r15 + TK_NODE_STACK]

	;remove task control block
	vp_cpy r15, r0
	vp_cpy r15, r1
	ln_remove_node r0, r15

	;add to suspend list and restore next
	vp_cpy tk_task_suspend_list, r0
	lh_add_at_head r0, r1, r2
	vp_jmp tk_restore_task

tk_resume_task:
	;inputs
	;r15 = task control node
	;r0 = task control node (to resume)
	;trashes
	;outputs
	;r15 = task control node
	;r0 = task control node (resumed)
	;trashes
	;r1-r2

	;remove task control block from suspend list
	vp_cpy r0, r1
	vp_cpy r0, r2
	ln_remove_node r1, r2

	;add to task list
	vp_cpy tk_task_list, r1
	lh_add_at_head r1, r0, r2
	vp_ret

;;;;;;;;;;;
; task data
;;;;;;;;;;;

	SECTION .data

	hp_heap_object tk_task_heap
	lh_list_object tk_task_list
	lh_list_object tk_task_suspend_list
