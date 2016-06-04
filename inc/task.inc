%ifndef TASK_1234
%define TASK_1234

%include 'inc/class.inc'
%include 'inc/list.inc'
%include 'inc/heap.inc'
%include 'inc/mail.inc'

;;;;;;;;;;;;;;;;;;;
; task static class
;;;;;;;;;;;;;;;;;;;

	def_class sys_task
	def_method statics, sys/task_statics
	def_method init, sys/task_init
	def_method deinit, sys/task_deinit
	def_method tcb, sys/task_tcb, static, {}, {r0}
	def_method mailbox, sys/task_mailbox, static, {}, {r0, r1}
	def_method callback, sys/task_callback, static, {r0, r1}
	def_method start, sys/task_start, static, {r0}, {r0, r1}
	def_method stop, sys/task_stop
	def_method restore, sys/task_restore
	def_method yield, sys/task_yield
	def_method count, sys/task_count, static, {}, {r0}
	def_method sleep, sys/task_sleep, static, {r0}
	def_method suspend, sys/task_suspend
	def_method resume, sys/task_resume, static, {r0}
	def_method open, sys/task_open, static, {r0}, {r0, r1}
	def_method open_child, sys/task_open_child, static, {r0}, {r0, r1}
	def_method open_device, sys/task_open_device, static, {r0, r1}, {r0, r1}
	def_method open_remote, sys/task_open_remote, static, {r0, r1}, {r0, r1}
	def_method open_array, sys/task_open_array, static, {r0, r1}
	def_method open_farm, sys/task_open_farm, static, {r0, r1, r2}
	def_method open_pipe, sys/task_open_pipe, static, {r0}, {r0}
	def_method open_global, sys/task_open_global, static, {r0, r1, r2}
	def_method stack_depth, sys/task_stack_depth, static, {}, {r0}

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

	tk_stack_size	equ 1024 * 4

	def_structure tk_node, ln_node
		ptr tk_node_stack
		ulong tk_node_time
		struct tk_node_mailbox, ml_mailbox
		struct tk_node_stackspace, tk_stack
	def_structure_end

	def_structure tk_statics
		ptr tk_statics_current_tcb
		ptr tk_statics_kernel_tcb
		struct tk_statics_task_heap, hp_heap
		struct tk_statics_task_list, lh_list
		struct tk_statics_timer_list, lh_list
		ulong tk_statics_task_count
		ulong tk_statics_cpu_total
		ulong tk_statics_cpu_id
	def_structure_end

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

	tk_state_size equ 15 * 8

	%macro tk_save_state 0
		vp_push r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15
	%endmacro

	%macro tk_load_state 0
		vp_pop r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15
	%endmacro

%endif
