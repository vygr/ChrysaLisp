(import 'inc/class.inc)
(import 'inc/list.inc)
(import 'inc/heap.inc)
(import 'inc/mail.inc)

;;;;;;;;;;;;;;;;;;;
; task static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_task)
(def-method 'statics 'sys/task_statics)
(def-method 'init 'sys/task_init)
(def-method 'deinit 'sys/task_deinit)
(def-method 'tcb 'sys/task_tcb 'static '() '(r0))
(def-method 'mailbox 'sys/task_mailbox 'static '() '(r0 r1))
(def-method 'callback 'sys/task_callback 'static '(r0 r1))
(def-method 'start 'sys/task_start 'static '(r0) '(r0 r1))
(def-method 'stop 'sys/task_stop)
(def-method 'restore 'sys/task_restore)
(def-method 'yield 'sys/task_yield)
(def-method 'count 'sys/task_count 'static '() '(r0))
(def-method 'sleep 'sys/task_sleep 'static '(r0))
(def-method 'suspend 'sys/task_suspend)
(def-method 'resume 'sys/task_resume 'static '(r0))
(def-method 'open 'sys/task_open 'static '(r0) '(r0 r1))
(def-method 'open_child 'sys/task_open_child 'static '(r0) '(r0 r1))
(def-method 'open_device 'sys/task_open_device 'static '(r0 r1) '(r0 r1))
(def-method 'open_remote 'sys/task_open_remote 'static '(r0 r1) '(r0 r1))
(def-method 'open_array 'sys/task_open_array 'static '(r0) '(r0))
(def-method 'open_farm 'sys/task_open_farm 'static '(r0 r1) '(r0))
(def-method 'open_pipe 'sys/task_open_pipe 'static '(r0) '(r0))
(def-method 'open_global 'sys/task_open_global 'static '(r0 r1) '(r0))

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

(equate 'tk_stack_size (mul 1024 4))

(def-struct 'tk_node 'ln_node)
	(ptr 'tk_node_stack)
	(ulong 'tk_node_time)
	(struct 'tk_node_mailbox 'mailbox)
	(struct 'tk_node_stackspace 'tk_stack)
(def-struct-end)

(def-struct 'tk_statics)
	(ptr 'tk_statics_current_tcb)
	(ptr 'tk_statics_kernel_tcb)
	(struct 'tk_statics_task_heap 'hp_heap)
	(struct 'tk_statics_task_list 'lh_list)
	(struct 'tk_statics_timer_list 'lh_list)
	(ulong 'tk_statics_task_count)
	(ulong 'tk_statics_cpu_total)
	(ulong 'tk_statics_cpu_id)
(def-struct-end)

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

(equate 'tk_state_size (mul 15 ptr_size)

(defun tk-save-state ()
	(vp-push r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))

(defun tk-load-state ()
	(vp-pop r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
