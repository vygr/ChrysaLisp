(import 'inc/class.inc)
(import 'inc/code.inc)
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
(def-method 'tcb 'sys/task_tcb 'static nil '(r0))
(def-method 'mailbox 'sys/task_mailbox 'static nil '(r0 r1))
(def-method 'callback 'sys/task_callback 'static '(r0 r1))
(def-method 'start 'sys/task_start 'static '(r0) '(r0 r1 r2))
(def-method 'stop 'sys/task_stop)
(def-method 'restore 'sys/task_restore)
(def-method 'yield 'sys/task_yield)
(def-method 'count 'sys/task_count 'static nil '(r0))
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

(defcvar 'tk_stack_size (mul 1024 4))

(def-struct 'tk_node 'ln_node)
	(ptr 'stack)
	(ulong 'time)
	(struct 'mailbox 'mailbox)
	(struct 'stackspace 'tk_stack)
(def-struct-end)

(def-struct 'tk_statics)
	(ptr 'current_tcb)
	(ptr 'kernel_tcb)
	(struct 'task_heap 'hp_heap)
	(struct 'task_list 'lh_list)
	(struct 'timer_list 'lh_list)
	(ulong 'task_count)
	(ulong 'cpu_total)
	(ulong 'cpu_id)
(def-struct-end)

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

(defcvar 'tk_state_size (mul 15 ptr_size))

(defcfun tk-save-state ()
	(vp-push r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))

(defcfun tk-load-state ()
	(vp-pop r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
