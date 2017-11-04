(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'sys/list.inc)
(import 'sys/heap.inc)
(import 'sys/mail.inc)

;;;;;;;;;;;;;;;;;;;
; task static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_task)
(def-method 'statics 'sys/task_statics)
(def-method 'init 'sys/task_init)
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
(def-method 'open_farm 'sys/task_open_farm 'static '(r0 r1 r2) '(r0))
(def-method 'open_pipe 'sys/task_open_pipe 'static '(r0) '(r0))

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
	(struct 'task_list 'lh_list)
	(struct 'timer_list 'lh_list)
	(ulong 'task_count)
	(ulong 'cpu_total)
	(ulong 'cpu_id)
(def-struct-end)

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

(defcfun tk-save-state ()
	(apply vp-push stack_state))

(defcfun tk-load-state ()
	(apply vp-pop stack_state))
