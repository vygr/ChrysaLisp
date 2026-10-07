;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; jobs, a queue of work handed out to a farm of children
;
; A job is a message. It starts with a +job, the key of the child it is
; given to and the mailbox the answer goes to, and what follows is the
; app's own. A child does the job and sends an answer that starts with
; a +job_reply, the key it was given. An empty message tells a child to
; go, and a child with nothing to do for a while goes of its own accord.
;
; The app has a mailbox for the word that a child has started and one
; for the answers, and passes what comes in on them to (:launched) and
; (:answered). A child that dies, or takes too long, is started again
; by (:refresh), and its job goes back on the queue.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./farm.inc")
(import "./local.inc")

(structure +job 0
	(long key)
	(netid reply))

(structure +job_reply 0
	(long key))

;module
(env-push)

;these are called by the farm, from inside a method of the jobs object,
;so this is the jobs object

(defun dispatch (key val)
	;the next job on the queue to a child, if there is one
	(cond
		((defq job (pop (get :queue this)))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val)
				(setf-> job
					(+job_key key)
					(+job_reply (get :reply_mbox this)))))
		(:t (undef val :job :timestamp))))

(defun create (key val nodes)
	;a child is started, the word that it has comes to the task mailbox
	(open-task (get :path this) (elem-get nodes (random (length nodes)))
		+kn_call_run key (get :task_mbox this)))

(defun destroy (key val)
	;a child is told to go, the job it had goes back on the queue
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		;and it is counted, a job that keeps coming back is one no
		;child can do
		(defq tried (get :tried this) body (slice job +job_size -1)
			n (inc (ifn (. tried :find body) 0)))
		(. tried :insert body n)
		(def this :tries (max n (get :tries this)))
		(push (get :queue this) job)
		(undef val :job)))

(defun idle ()
	;every child that is up and has no job is given one
	(. (get :farm this) :each (lambda (key val)
		(if (and (get :child val) (not (get :job val)))
			(dispatch key val)))))

(defclass Jobs (path task_mbox reply_mbox &optional size) :nil
	; (Jobs path task_mbox reply_mbox [size]) -> jobs
	;children of the task at path, a node's worth of them if no size is
	;given, on any of the nodes. A size that is a list, (herd_max
	;[herd_init herd_growth]), is a herd on the nodes of this machine,
	;those that share its file system, as (Local) has it. They are
	;started at once.
	(def this :path path :task_mbox task_mbox :reply_mbox reply_mbox
		:queue (list) :out 0 :tried (Fmap) :tries 0)
	(def this :farm (if (list? size)
		(apply (const Local) (cat (list (const create) (const destroy)) size))
		(Farm (const create) (const destroy) (ifn size (max 1 (length (lisp-nodes)))))))

	(defmethod :add (jobs)
		; (. jobs :add jobs) -> jobs
		;more jobs for the queue, the last of them is the first to go
		(def this :out (+ (get :out this) (length jobs)))
		(each (# (push (get :queue this) %0)) jobs)
		(idle)
		this)

	(defmethod :out ()
		; (. jobs :out) -> num
		;how many jobs are not yet answered
		(get :out this))

	(defmethod :launched (msg)
		; (. jobs :launched msg) -> jobs
		;a child has started, msg is what came to the task mailbox
		(defq key (getf msg +kn_msg_key))
		(when (defq val (. (get :farm this) :find key))
			(def val :child (getf msg +kn_msg_reply_id))
			(dispatch key val))
		this)

	(defmethod :answered (msg)
		; (. jobs :answered msg) -> :nil | num
		;a job is done, msg is what came to the reply mailbox. How many
		;are still not answered is the result. :nil if the answer is
		;from a child that has since been started again, its job went
		;back on the queue and the answer is to be left alone.
		(defq key (getf msg +job_reply_key))
		(when (and (defq val (. (get :farm this) :find key)) (get :job val))
			(def this :out (dec (get :out this)))
			(dispatch key val)
			(get :out this)))

	(defmethod :job (msg)
		; (. jobs :job msg) -> :nil | job
		;the job an answer is to, msg is what came to the reply mailbox,
		;asked before it is passed to (:answered). :nil if the answer
		;is from a child that has since been started again.
		(and (defq val (. (get :farm this) :find (getf msg +job_reply_key)))
			(get :job val)))

	(defmethod :failed (msg)
		; (. jobs :failed msg) -> :nil | num
		;as (:answered), for a job that went wrong. It is not put back
		;on the queue, and its child is started again.
		(defq key (getf msg +job_reply_key) farm (get :farm this))
		(when (and (defq val (. farm :find key)) (get :job val))
			(def this :out (dec (get :out this)))
			(undef val :job :timestamp)
			(. farm :restart key val)
			(get :out this)))

	(defmethod :clear ()
		; (. jobs :clear) -> jobs
		;the queue is emptied, the jobs the children have now are left
		;to finish and are all that is out
		(defq n 0)
		(. (get :farm this) :each (lambda (key val) (if (get :job val) (setq n (inc n)))))
		(def this :queue (list) :out n :tried (Fmap) :tries 0)
		this)

	(defmethod :restart ()
		; (. jobs :restart) -> jobs
		;every child is started again, and the queue is emptied. A child
		;with nothing to do for a while has gone, and one part way
		;through a job that is no longer wanted is told to stop.
		(defq farm (get :farm this) keys (list) vals (list))
		(. farm :each (# (undef %1 :job) (push keys %0) (push vals %1)))
		(each (# (. farm :restart %0 %1)) keys vals)
		(def this :queue (list) :out 0 :tried (Fmap) :tries 0)
		this)

	(defmethod :refresh (&optional timeout)
		; (. jobs :refresh [timeout]) -> :t | :nil
		;start again any child that has gone, or has had its job for
		;longer than the timeout
		(. (get :farm this) :refresh timeout))

	(defmethod :tries ()
		; (. jobs :tries) -> num
		;the most times any one job has been put back on the queue, its
		;child gone or too long over it. A job that no child can do, or
		;a child that can not start, shows as this going up and up.
		(get :tries this))

	(defmethod :size ()
		; (. jobs :size) -> num
		;how many children there are
		(defq n 0)
		(. (get :farm this) :each (lambda (key val) (setq n (inc n))))
		n)

	(defmethod :close ()
		; (. jobs :close) -> jobs
		;every child is told to go
		(. (get :farm this) :close)
		(def this :queue (list) :out 0 :tried (Fmap) :tries 0)
		this)
	)

;module
(export-classes '(Jobs))
(env-pop)
