;;;;;;;;;;;;;;;;;;
; pipe farm object
;;;;;;;;;;;;;;;;;;

(import "./jobs.inc")

;module
(env-push)

(defun pipe-farm (cmds &optional retry_timeout told)
	; (pipe-farm jobs [retry_timeout told]) -> ((job result) ...)
	;run pipe farm and collect output. A job that is not answered within
	;retry_timeout, in microseconds, a minute if not given, is given to a
	;new worker, and so is one whose node has gone. A job that has been
	;given out three times with no answer stops the farm, and it and any
	;that are still out have no result. With told the farm says how it
	;goes as it goes, (told :start job) and (told :done job result), see
	;(Monitored), and (told :tick) each second, so it can say what is late:
	;what that gives back, a list of the jobs heard from since, has its
	;time start again, so a job is given again when its worker has said
	;nothing for retry_timeout, not when it is slow
	(task-count -1) ;we will be only collating results !
	(setd retry_timeout 60000000)
	(defq results (cap (length cmds) (list))
		;the timeout is longer on the emulator, as (task-timeout) has it
		retry_timeout (* retry_timeout (/ (task-timeout 1) 1000000)))
	(if (nempty? cmds)
		(jobs-farm "lib/task/cmd.lisp" (map (# (cat (const (str-alloc +job_size)) %0)) cmds)
			(list (length cmds) 10) retry_timeout
			(lambda (msg jobs)
				(when (defq job (. jobs :job msg))
					(push results (list (slice job +job_size -1) (slice msg +job_reply_size -1)))
					(. jobs :answered msg)))
			:nil told))
	(task-count 1)	;we where only collating results !
	results)

;module
(export-symbols '(pipe-farm))
(env-pop)
