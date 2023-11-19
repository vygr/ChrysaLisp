;;;;;;;;;;;;;;;;;;
; pipe farm object
;;;;;;;;;;;;;;;;;;

(import "./local.inc")

;module
(env-push)

(defq +LF (ascii-char 10))

(enums +select 0
	(enum task reply timer))

(defun dispatch-job (key val)
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val) (cat
				(char key +long_size) (elem-get +select_reply select) job)))
		(:t ;no jobs in que
			(undef val :job :timestamp))))

(defun create (key val nodes)
	(open-task "lib/task/cmd.lisp" (elem-get (random (length nodes)) nodes)
		+kn_call_child key (elem-get +select_task select)))

(defun destroy (key val)
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(print "Restarting cmd job ! -> " job)
		(push jobs job)
		(undef val :job :timestamp)))

(defun pipe-farm (jobs &optional retry_timeout)
	; (pipe-farm jobs &optional retry_timeout) -> ((job result) ...)
	;run pipe farm and collect output
	(setd retry_timeout 1000000)
	(defq timer_rate (/ 1000000 1) working :t results (cap (length jobs) (list))
		retry_timeout (if (starts-with "obj/vp64" (load-path)) (* 10 retry_timeout) retry_timeout)
		select (list (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
		farm (Local (const create) (const destroy) (length jobs) (max 1 (min 4 (length (lisp-nodes))))))
	(mail-timeout (elem-get +select_timer select) timer_rate 0)
	(while working
		(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_task)
				;child launch responce
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(. farm :add_node (slice +mailbox_id_size -1 child))
					(def val :child child)
					(dispatch-job key val)))
			((= idx +select_reply)
				;child worker responce
				(defq key_pos (find +LF msg)
					key (str-as-num (slice 0 key_pos msg))
					msg (slice (inc key_pos) -1 msg))
				(when (defq val (. farm :find key))
					(push results (list (get :job val) msg))
					(dispatch-job key val))
				;all jobs done ?
				(when (= 0 (length jobs))
					(setq working :nil)
					(. farm :each (lambda (key val)
						(setq working (or working (get :job val)))))))
			(:t ;timer event
				(mail-timeout (elem-get +select_timer select) timer_rate 0)
				(. farm :refresh retry_timeout))))
	(. farm :close)
	results)

;module
(export-symbols '(pipe-farm))
(env-pop)
