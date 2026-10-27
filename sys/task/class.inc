(include "././list/class.inc")
(include "././heap/class.inc")
(include "././mail/class.inc")
(include "./struct.inc")

;;;;;;;;;;;;;;;;;;;
; task static class
;;;;;;;;;;;;;;;;;;;

(def-class :sys_task :nil
	(dec-method :statics_init sys/task/statics_init :static :nil :nil (:r0-:r2))
	(dec-method :mailbox sys/task/mailbox :static :nil (:r0 :r1 :r2) (:r0-:r2))
	(dec-method :callback sys/task/callback :static (:r0 :r1) :nil (:r0-:r14 :f0-:f15))
	(dec-method :count sys/task/count :static (:r0) (:r0) (:r0-:r2))
	(dec-method :start sys/task/start :static (:r0) (:r0 :r1 :r2 :r3 :r4) (:r0-:r6 :f0-:f15))
	(dec-method :stop sys/task/stop :static :nil :nil (:r0-:r14 :f0-:f15))
	(dec-method :restore sys/task/restore :static :nil :nil (:r0-:r14 :f0-:f15))
	(dec-method :sleep sys/task/sleep :static (:r0) :nil (:r0 :f0-:f15))
	(dec-method :slice sys/task/slice :static :nil :nil (:r0-:r2 :f0-:f15))
	(dec-method :suspend sys/task/suspend :static :nil :nil (:r0-:r14 :f0-:f15))
	(dec-method :resume sys/task/resume :static (:r0) (:r0) (:r1-:r2))
	(dec-method :defer sys/task/defer :static (:r0) :nil (:r0-:r14 :f0-:f15))
	(dec-method :set_priority sys/task/set_priority :static (:r0) :nil (:r0-:r4))
	(dec-method :timer sys/task/timer :static :nil (:r0) (:r0-:r7 :f0-:f15))
	(dec-method :wake_links sys/task/wake_links :static :nil :nil (:r0-:r3))
	(dec-method :dump sys/task/dump :static :nil :nil ())
	(dec-method :stack sys/task/stack :static :nil :nil ())
	(dec-method :stacks sys/task/stacks :static :nil (:r0) (:r0-:r9))
	(dec-method :task_callback class/obj/null :static (:r0) :nil ())
	(dec-method :lisp_count sys/task/lisp_count :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :r14 :f0-:f15))
	(dec-method :lisp_sleep sys/task/lisp_sleep :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :f0-:f15))
	(dec-method :lisp_timeslice sys/task/lisp_timeslice :static (:r0 :r1) (:r0 :r1) (:r1-:r3 :f0-:f15))
	(dec-method :lisp_mailbox sys/task/lisp_mailbox :static (:r0 :r1) (:r0 :r1) (:r1-:r5 :f0-:f15))
	(dec-method :lisp_flags sys/task/lisp_flags :static (:r0 :r1) (:r0 :r1) (:r1-:r3 :r14 :f0-:f15))
	(dec-method :lisp_priority sys/task/lisp_priority :static (:r0 :r1) (:r0 :r1) (:r1-:r6))
	(dec-method :lisp_apps sys/task/lisp_apps :static (:r0 :r1) (:r0 :r1) (:r1-:r3)))

;;;;;;;;;;;;;;;;;
; task structures
;;;;;;;;;;;;;;;;;

;number of distinct priorities, bands. A task of a band runs only when
;no task of a band before it is ready, and those of a band take turns
;0 Kernel task
;1 Out Link tasks and Postman task
;2 In Link tasks
;3 GUI task
;4 Bots, tasks that work by themselves for others: the Storage
;  services, their tenders and gophers. Measured, 2026-10-14, 300
;  gophers on 8 nodes with each node busy with apps: 16 s with them in
;  the band of apps, 0.19 s in this one, as with no apps at all.
;  (task-band :bot), sys/lisp.inc
;5 Application tasks

(defq tk_num_task_lists 6 tk_kernel_priority 0
	tk_out_link_priority 1 tk_in_link_priority 2
	tk_gui_priority 3 tk_bot_priority 4 tk_app_priority (dec tk_num_task_lists)
	tk_stack_size (* 1024 6) tk_stack_margin (* 512 1)
	;with less of its stack left than this, a task does not destroy an
	;object there and then: (:obj :destroy)
	tk_stack_low (* 512 3)
	tk_stack_canary 0xbadf00d123456789)

;;;;;;;;;;;;;
; task macros
;;;;;;;;;;;;;

(defun tk-validate-trash ()
	;validate mode trashes all registers, before a task's state, (vp-load-state)
	(validatecase
		(vp-cpy-cr 0x7FF8000000000000 :r0)
		(vp-simd vp-cpy-rf '(:r0) +vp_fregs)
		(vp-simd vp-cpy-rr '(:r0) +vp_rregs_0_14)))
