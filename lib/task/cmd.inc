;;;;;;;;;;;;;;;;;;
; pipe farm object
;;;;;;;;;;;;;;;;;;

(import "./local.inc")

;module
(env-push)

(enums +select 0
	(enum task reply timer))

(defun dispatch-job (key val)
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val) (cat
				(char key +long_size) (elem-get select +select_reply) job)))
		(:t ;no jobs in que
			(undef val :job :timestamp))))

(defun create (key val nodes)
	(open-task "lib/task/cmd.lisp" (elem-get nodes (random (length nodes)))
		+kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(print "Restarting cmd job ! -> " job)
		(push jobs job)
		(undef val :job :timestamp)))

(defun pipe-farm (jobs &optional retry_timeout)
	; (pipe-farm jobs [retry_timeout]) -> ((job result) ...)
	;run pipe farm and collect output
	(task-count -1)	;we will be only collating results !
	(setd retry_timeout 1000000)
	(defq timer_rate (/ 1000000 1) working :t results (cap (length jobs) (list))
		retry_timeout (if (starts-with "obj/vp64" (load-path)) (* 10 retry_timeout) retry_timeout)
		select (list (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
		farm (Local (const create) (const destroy) (length jobs) (max 1 (min 4 (length (lisp-nodes))))))
	(mail-timeout (elem-get select +select_timer) timer_rate 0)
	(while working
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_task)
				;child launch response
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(. farm :add_node (slice child +mailbox_id_size -1))
					(def val :child child)
					(dispatch-job key val)))
			((= idx +select_reply)
				;child worker response
				(defq key_pos (find (ascii-char 10) msg)
					key (str-as-num (slice msg 0 key_pos))
					msg (slice msg (inc key_pos) -1))
				(when (defq val (. farm :find key))
					(push results (list (get :job val) msg))
					(dispatch-job key val))
				;all jobs done ?
				(when (= 0 (length jobs))
					(setq working :nil)
					(. farm :each (lambda (key val)
						(setq working (or working (get :job val)))))))
			(:t ;timer event
				(mail-timeout (elem-get select +select_timer) timer_rate 0)
				(. farm :refresh retry_timeout))))
	(. farm :close)
	(task-count 1)	;we where only collating results !
	results)

;module
(export-symbols '(pipe-farm))
(env-pop)
