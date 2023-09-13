;;;;;;;;;;;;;;
; match? Child
;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")

(defq +_timeout 5000000)

(enums +_select 0
	(enum main timeout))

(structure +_job 0
	(long key)
	(netid reply)
	(int result)
	(offset params))

(defun file-match? (file search pattern meta)
	(when (defq result :nil stream (file-stream file))
		(while (and (not result) (defq line (read-line stream)))
			(task-slice)
			(setq result (. search :match? line pattern meta)))
		result))

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
					result 0)
				(bind '(file pattern whole_words regexp)
					(elem-get 0 (read (string-stream (slice +_job_params -1 *msg*)) (ascii-code " "))))
				(when (defq search (if regexp +*regexp* +*substr*)
						pattern (map (const progn) pattern)
						meta (. search :compile pattern))
					(setq result (if (file-match? file search pattern meta) 1 0)))
				;send reply
				(mail-send *reply_mbox* (setf (slice 0 +_job_params *msg*)
					+_job_result result)))))
	(free-select *select*))
