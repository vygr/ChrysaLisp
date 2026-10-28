;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, one of the farm that puts a folder in a store: ./tree.lisp
;
; A job is a file. What follows the job's header is four lines: p or v,
; the service to ask, the path, and the file of the host. For p the file
; is put at the path, as a stream. For v the object of that path is read
; as a stream and compared with the file. The answer is 1, and for v how
; long it was, or 0 and why not.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")
(import "service/storage/app.inc")

(defun main ()
	;one with nothing to do for a while goes
	(job-child 5000000 (lambda (msg mbox)
		(bind '(what svc path file) (split (slice msg +job_size -1) "\n"))
		(defq svc (hex-decode svc)
			answer (catch (cond
					((eql what "p")
						(if (eql (defq how (first (storage-put-file path file :nil svc))) :ok) "1"
							(cat "0 the store said " (rest (str how)))))
					((defq size (storage-same-file? path file svc)) (cat "1 " (str size)))
					("0 it is not the same"))
				(cat "0 " (slice (str _) 0 (min 120 (length (str _)))))))
		(mail-send (getf msg +job_reply)
			(cat (setf-> (str-alloc +job_reply_size) (+job_reply_key (getf msg +job_key))) answer)))))
