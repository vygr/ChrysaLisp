(include "././list/class.inc")
(include "././heap/class.inc")
(include "././mail/class.inc")

;;;;;;;;;;;;;;;;;;;
; task static class
;;;;;;;;;;;;;;;;;;;

(def-class sys_task :nil
	(dec-method :statics_init sys/task/statics_init)
	(dec-method :mailbox sys/task/mailbox :static :nil (:r0 :r1 :r2))
	(dec-method :callback sys/task/callback :static (:r0 :r1))
	(dec-method :count sys/task/count :static (:r0) (:r0))
	(dec-method :start sys/task/start :static (:r0) (:r0 :r1 :r2 :r3 :r4))
	(dec-method :stop sys/task/stop)
	(dec-method :restore sys/task/restore)
	(dec-method :sleep sys/task/sleep :static (:r0))
	(dec-method :suspend sys/task/suspend)
	(dec-method :resume sys/task/resume :static (:r0) (:r0))
	(dec-method :defer sys/task/defer :static (:r0))
	(dec-method :set_priority sys/task/set_priority :static (:r0))
	(dec-method :timer sys/task/timer :static :nil (:r0))
	(dec-method :stacks sys/task/stacks :static :nil (:r0))
	(dec-method :dump sys/task/dump)
	(dec-method :task_callback class/obj/null :static (:r0))
	(dec-method :lisp_count sys/task/lisp_count :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_sleep sys/task/lisp_sleep :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_timeslice sys/task/lisp_timeslice :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_mailbox sys/task/lisp_mailbox :static (:r0 :r1) (:r0 :r1)))

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

(defcvar 'tk_stack_size (* 1024 10))
(defcvar 'tk_stack_margin (* 512 1))
(defcvar 'tk_stack_canary 0xbadf00d123456789)

(def-struct tk_node ln_node_size
	(ptr stack stack_top mbox_addr lisp tls)
	(ulong time)
	(uint priority mbox_id))

;number of distinct priorities
;currently experimenting with
;0 Kernel process and interrupts
;1 Link Drivers and Postman process
;2 GUI process
;3 Applications
(defcvar 'num_task_lists 4)

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

(defun tk-save-state ()
	(apply vp-push stack_state))

(defun tk-load-state ()
	(apply vp-pop stack_state))
