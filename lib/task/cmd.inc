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
	;run pipe farm and collect output. A job that is not answered within
	;retry_timeout, in microseconds, a minute if not given, is given to a
	;new worker, and so is one whose node has gone. A job that has been
	;given out three times with no answer stops the farm, and it and any
	;that are still out have no result.
	(task-count -1) ;we will be only collating results !
	(setd retry_timeout 60000000)
	(defq timer_rate (/ 1000000 1) working (nempty? cmds) results (cap (length cmds) (list))
		;the timeout is longer on the emulator, as (task-timeout) has it
		retry_timeout (* retry_timeout (/ (task-timeout 1) 1000000))
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
				(. jobs :refresh retry_timeout)
				(if (>= (. jobs :tries) 3) (setq working :nil)))))
	(. jobs :close)
	(task-count 1)	;we where only collating results !
	results)

;module
(export-symbols '(pipe-farm))
(env-pop)
