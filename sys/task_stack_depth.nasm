%include 'inc/func.inc'
%include 'inc/task.inc'

def_func sys/task_stack_depth
	;outputs
	;r0 = stack depth (in bytes)
	;trashes
	;r0

	f_bind sys_task, statics, r0
	vp_cpy [r0 + tk_statics_current_tcb], r0
	vp_add tk_node_stackspace, r0
	if r4, <, r0
		;must be kernel or stack extended
		vp_xor r0, r0
		vp_ret
	endif
	vp_add tk_stack_size, r0
	vp_sub r4, r0
	if r0, >, tk_stack_size
		;must be kernel or stack extended
		vp_xor r0, r0
	endif
	vp_ret

def_func_end
