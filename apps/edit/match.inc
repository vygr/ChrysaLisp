;;;;;;;;;;;;;;;
; global match?
;;;;;;;;;;;;;;;

;module
(env-push)

(structure +job 0
	(long key)
	(netid reply)
	(int result)
	(offset params))

(enums +select 0
	(enum task reply timer))

(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val)
				(setf-> (cat (str-alloc +job_size) (str job))
					(+job_key key)
					(+job_reply (elem-get +select_reply select)))))
		(:t ;no jobs in que
			(undef val :job :timestamp))))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "apps/edit/match.lisp" (elem-get (random (length nodes)) nodes)
		+kn_call_child key (elem-get +select_task select)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(push jobs job)
		(undef val :job :timestamp)))

(defun file-matches (files pattern whole_words regexp)
	(when (nempty? files)
		(defq timer_rate (/ 1000000 1) working :t
			retry_timeout (if (starts-with "obj/vp64" (load-path)) 20000000 2000000)
			select (list (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
			jobs (map (# (list %0 (encode pattern) whole_words regexp)) files)
			farm (Local (const create) (const destroy) (length files) 4)
			files (list))
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
					(defq key (getf msg +job_key)
						result (getf msg +job_result))
					(when (defq val (. farm :find key))
						(if (/= result 0) (push files (first (get :job val))))
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
		(each mail-free-mbox select))
	files)

;module
(export-symbols file-matches)
(env-pop)
