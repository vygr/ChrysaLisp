(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'sys/list/list.inc)
(import 'sys/heap/heap.inc)
(import 'sys/mail/mail.inc)

;;;;;;;;;;;;;;;;;;;
; task static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_task)
(def-method 'statics 'sys/task/statics)
(def-method 'init 'sys/task/init)
(def-method 'tcb 'sys/task/tcb 'static nil '(r0))
(def-method 'mailbox 'sys/task/mailbox 'static nil '(r0 r1))
(def-method 'callback 'sys/task/callback 'static '(r0 r1))
(def-method 'start 'sys/task/start 'static '(r0) '(r0 r1 r2))
(def-method 'stop 'sys/task/stop)
(def-method 'restore 'sys/task/restore)
(def-method 'yield 'sys/task/yield)
(def-method 'count 'sys/task/count 'static nil '(r0))
(def-method 'sleep 'sys/task/sleep 'static '(r0))
(def-method 'suspend 'sys/task/suspend)
(def-method 'resume 'sys/task/resume 'static '(r0))
(def-method 'open_child 'sys/task/open_child 'static '(r0 r1) '(r0 r1))
(def-method 'open_remote 'sys/task/open_remote 'static '(r0 r1 r2) '(r0 r1))
(def-method 'open_farm 'sys/task/open_farm 'static '(r0 r1 r2) '(r0))
(def-method 'open_pipe 'sys/task/open_pipe 'static '(r0) '(r0))

(def-method 'lisp_mailbox 'sys/task/lisp_mailbox 'static '(r0 r1) '(r0 r1))
(def-method 'lisp_open_child 'sys/task/lisp_open_child 'static '(r0 r1) '(r0 r1))
(def-method 'lisp_open_remote 'sys/task/lisp_open_remote 'static '(r0 r1) '(r0 r1))
(def-method 'lisp_open_farm 'sys/task/lisp_open_farm 'static '(r0 r1) '(r0 r1))
(def-method 'lisp_open_pipe 'sys/task/lisp_open_pipe 'static '(r0 r1) '(r0 r1))

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

(defcvar 'tk_stack_size (mul 1024 4))

(def-struct 'tk_node 'ln_node)
	(ptr 'stack)
	(ulong 'time)
	(ptr 'mbox_addr)
	(uint 'mbox_id)
	(local-align)
	(struct 'stackspace 'tk_stack)
(def-struct-end)

(def-struct 'tk_statics)
	(ptr 'current_tcb)
	(ptr 'kernel_tcb)
	(struct 'timer_list 'lh_list)
	(uint 'task_count)
	(uint 'cpu_total)
	(uint 'cpu_id)
(def-struct-end)

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

(defcfun tk-save-state ()
	(apply vp-push stack_state))

(defcfun tk-load-state ()
	(apply vp-pop stack_state))
