;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, one of the farm that puts a folder in a store: ./tree.lisp
;
; A job is a file. What follows the job's header is four lines: p or v,
; the service to ask, the path, and the file of the host. For p the file
; is put at the path, as a stream. For v the object of that path is read
; as a stream and compared with the file. The answer is 1, and for v how
; long it was, or 0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")
(import "service/storage/app.inc")

(enums +select 0
	(enum main timeout))

(defun main ()
	(defq select (task-mboxes +select_size) running :t)
	(while running
		;one with nothing to do for a while goes
		(mail-timeout (elem-get select +select_timeout) 5000000 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg "")) (setq running :nil))
			(:t (mail-timeout (elem-get select +select_timeout) 0 0)
				(bind '(what svc path file) (split (slice msg +job_size -1) (ascii-char 10)))
				(defq svc (hex-decode svc)
					answer (catch (cond
							((eql what "p") (if (eql (first (storage-put-file path file :nil svc)) :ok) "1" "0"))
							((defq size (storage-same-file? path file svc)) (cat "1 " (str size)))
							("0"))
						"0"))
				(mail-send (getf msg +job_reply)
					(cat (setf-> (str-alloc +job_reply_size) (+job_reply_key (getf msg +job_key))) answer))))))
