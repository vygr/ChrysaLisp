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
	def_method tcb, sys/task_tcb
	def_method mailbox, sys/task_mailbox
	def_method callback, sys/task_callback
	def_method start, sys/task_start
	def_method stop, sys/task_stop
	def_method restore, sys/task_restore
	def_method yield, sys/task_yield
	def_method count, sys/task_count
	def_method sleep, sys/task_sleep
	def_method suspend, sys/task_suspend
	def_method resume, sys/task_resume
	def_method open, sys/task_open
	def_method open_child, sys/task_open_child
	def_method open_device, sys/task_open_device
	def_method open_remote, sys/task_open_remote
	def_method open_array, sys/task_open_array
	def_method open_farm, sys/task_open_farm
	def_method open_pipe, sys/task_open_pipe
	def_method open_global, sys/task_open_global
	def_method stack_depth, sys/task_stack_depth

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

	%assign tk_stack_size 1024 * 4

	def_structure	tk_node, ln_node
		def_long	tk_node_stack
		def_long	tk_node_time
		def_struct	tk_node_mailbox, ml_mailbox
		def_struct	tk_node_stackspace, tk_stack
	def_structure_end

	def_structure	tk_statics
		def_long	tk_statics_current_tcb
		def_long	tk_statics_kernel_tcb
		def_struct	tk_statics_task_heap, hp_heap
		def_struct	tk_statics_task_list, lh_list
		def_struct	tk_statics_timer_list, lh_list
		def_long	tk_statics_task_count
		def_long	tk_statics_cpu_total
		def_long	tk_statics_cpu_id
	def_structure_end

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

	tk_state_size 	equ 15 * 8

	%macro tk_save_state 0
		vp_push r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15
	%endmacro

	%macro tk_load_state 0
		vp_pop r0, r1, r2, r3, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15
	%endmacro

%endif
