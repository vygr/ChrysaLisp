;;;;;;;;;;;
; Cmd Child
;;;;;;;;;;;

(import "lib/task/pipe.inc")

(defq +_timeout 5000000)

(enums +_select 0
	(enum main timeout))

(structure +_job 0
	(long key)
	(netid reply)
	(offset params))

(defun main ()
	(defq *select* (alloc-select +_select_size) *working* :t *msg* :nil)
	(while *working*
		(mail-timeout (elem-get +_select_timeout *select*) +_timeout 0)
		(setq *msg* (mail-read (elem-get (defq *idx* (mail-select *select*)) *select*)))
		(cond
			;timeout or quit
			((or (= *idx* +_select_timeout) (eql *msg* ""))
				(setq *working* :nil))
			;main mailbox
			((= *idx* +_select_main)
				;clear timeout
				(mail-timeout (elem-get +_select_timeout *select*) 0 0)
				;read job
				(defq *reply_key* (getf *msg* +_job_key)
					*reply_mbox* (getf *msg* +_job_reply)
					*cmd* (slice +_job_params -1 *msg*))
				;run the command and catch output
				(setq *msg* (string-stream (cat "")))
				(write-line *msg* (str *reply_key*))
				(catch (pipe-run *cmd* (# (write *msg* (str %0))))
					(progn (write-line *msg* (str _ " " *cmd*)) :t))
				;send reply
				(mail-send *reply_mbox* (str *msg*)))))
	(free-select *select*))
