;;;;;;;;;;;
; Cmd Child
;;;;;;;;;;;

(import "./pipe.inc")

;a worker with no job for this long ends, its farm has gone and did not
;say. It must be long, the farm's node may be busy, and a job sent to a
;worker that has ended is lost.
(defq +timeout (task-timeout 60))

(enums +select 0
	(enum main timeout))

(structure +job 0
	(long key)
	(netid reply)
	(offset params))

(defun main ()
	(defq *select* (task-mboxes +select_size) *working* :t *msg* :nil)
	(while *working*
		(mail-timeout (elem-get *select* +select_timeout) +timeout 0)
		(setq *msg* (mail-read (elem-get *select* (defq *idx* (mail-select *select*)))))
		(cond
			;timeout or quit
			((or (= *idx* +select_timeout) (eql *msg* ""))
				(setq *working* :nil))
			;main mailbox
			((= *idx* +select_main)
				;clear timeout
				(mail-timeout (elem-get *select* +select_timeout) 0 0)
				;read job
				(defq *reply_key* (getf *msg* +job_key)
					*reply_mbox* (getf *msg* +job_reply)
					*cmd* (slice *msg* +job_params -1))
				;run the command and catch output
				;the reply is the key it was given, then what the command said
				(setq *msg* (string-stream (cat "")))
				(write-blk *msg* (char *reply_key* +long_size))
				(catch (pipe-run *cmd* (# (write-blk *msg* (str %0))))
					(progn (write-line *msg* (str _ " " *cmd*)) :t))
				;send reply
				(mail-send *reply_mbox* (str *msg*))))))
