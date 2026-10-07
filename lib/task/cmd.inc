;;;;;;;;;;;;;;;;;;
; pipe farm object
;;;;;;;;;;;;;;;;;;

(import "./jobs.inc")

;module
(env-push)

(enums +select 0
	(enum task reply timer))

(defun pipe-farm (cmds &optional retry_timeout)
	; (pipe-farm jobs [retry_timeout]) -> ((job result) ...)
	;run pipe farm and collect output
	(task-count -1) ;we will be only collating results !
	(setd retry_timeout 1000000)
	(defq timer_rate (/ 1000000 1) working (nempty? cmds) results (cap (length cmds) (list))
		retry_timeout (task-timeout retry_timeout)
		select (list (mail-mbox) (mail-mbox) (mail-mbox))
		jobs (Jobs "lib/task/cmd.lisp" (elem-get select +select_task)
			(elem-get select +select_reply) (list (length cmds) 10)))
	(. jobs :add (map (# (cat (const (str-alloc +job_size)) %0)) cmds))
	(mail-timeout (elem-get select +select_timer) timer_rate 0)
	(while working
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((not msg)
				;task aborted
				(setq working :nil))
			((= idx +select_task)
				;child launch response
				(. jobs :launched msg))
			((= idx +select_reply)
				;child worker response
				(when (defq job (. jobs :job msg))
					(push results (list (slice job +job_size -1) (slice msg +job_reply_size -1)))
					;all jobs done ?
					(if (= 0 (. jobs :answered msg)) (setq working :nil))))
			(:t ;timer event
				(mail-timeout (elem-get select +select_timer) timer_rate 0)
				(. jobs :refresh retry_timeout))))
	(. jobs :close)
	(task-count 1)	;we where only collating results !
	results)

;module
(export-symbols '(pipe-farm))
(env-pop)
