(include "././list/class.inc")
(include "././heap/class.inc")
(include "././mail/class.inc")
(include "./struct.inc")

;;;;;;;;;;;;;;;;;;;
; task static class
;;;;;;;;;;;;;;;;;;;

(def-class :sys_task :nil
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
	(dec-method :wake_links sys/task/wake_links)
	(dec-method :dump sys/task/dump)
	(dec-method :stack sys/task/stack)
	(dec-method :stacks sys/task/stacks :static :nil (:r0))
	(dec-method :task_callback class/obj/null :static (:r0))
	(dec-method :lisp_count sys/task/lisp_count :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_sleep sys/task/lisp_sleep :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_timeslice sys/task/lisp_timeslice :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_mailbox sys/task/lisp_mailbox :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_flags sys/task/lisp_flags :static (:r0 :r1) (:r0 :r1)))

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

;number of distinct priorities
;currently experimenting with
;0 Kernel task
;1 Out Link tasks and Postman task
;2 In Link tasks
;3 GUI task
;4 Application tasks

(defq tk_num_task_lists 5 tk_kernel_priority 0
	tk_out_link_priority 1 tk_in_link_priority 2
	tk_gui_priority 3 tk_app_priority (dec tk_num_task_lists)
	tk_stack_size (* 1024 6) tk_stack_margin (* 512 1)
	tk_stack_canary 0xbadf00d123456789)

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

(defun tk-save-state ()
	(apply (const vp-push) stack_state))

(defun tk-load-state ()
(validatecase
	;validate mode trashes all registers !
	(vp-cpy-cr 0x7FF8000000000000 :r0)
	(vp-simd vp-cpy-rf '(:r0) +vp_fregs)
	(vp-simd vp-cpy-rr '(:r0) +vp_rregs_0_14))
	(apply (const vp-pop) stack_state))
