%include 'inc/func.inc'
%include 'inc/task.inc'

def_func sys/task_start
	;inputs
	;r0 = new task func pointer
	;outputs
	;r0 = new task control block
	;r1 = new task mailbox
	;trashes
	;r2-r3, r5

	;save prog counter
	vp_cpy r0, r5

	;increment task count
	f_bind sys_task, statics, r0
	vp_cpy [r0 + tk_statics_task_count], r1
	vp_inc r1
	vp_cpy r1, [r0 + tk_statics_task_count]

	;create new task control block and task
;	vp_cpy r5, r2
;	vp_cpy_ub [r2 - 1], r1
;	vp_sub r1, r2
;	vp_cpy_ui [r2 - int_size], r1
;	if r1, >, tk_stack_size
;		vp_add tk_node_stackspace, r1
;		f_call sys_mem, alloc, {r1}, {r0, _}
;	else
		vp_add tk_statics_task_heap, r0
		f_call sys_heap, alloc, {r0}, {r1}
		vp_cpy r0, [r1]
		vp_lea [r1 + ptr_size], r0
;	endif

	;initialise task mailbox etc
	vp_xor r1, r1
	vp_cpy r1, [r0]
	vp_lea [r0 + tk_node_mailbox], r1
	ml_init r1, r2, r3

	;set task control block stack and return address's
	vp_lea [r0 + tk_node_size], r1
	vp_sub tk_state_size + 16, r1
	vp_cpy r1, [r0 + tk_node_stack]
	fn_bind sys/task_stop, r2
	vp_cpy r2, [r1 + tk_state_size + 8]
	vp_cpy r5, [r1 + tk_state_size]

	;resume new task
	f_call sys_task, resume, {r0}

	;return mailbox pointer
	vp_lea [r0 + tk_node_mailbox], r1
	vp_ret

def_func_end
