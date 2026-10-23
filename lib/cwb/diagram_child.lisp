;a child of make docs, cmd/make.lisp, that makes diagrams of the reference
;and saves them, a job a list of them. Each is (:class file args ...) or
;(:tree file args ...), the args of (dia-class) or (dia-hierarchy), or
;(:script file), a script of docs/diagrams/ that makes its own and saves it
;by (diagram name doc). What went wrong is the answer, "" for nothing. It
;goes when told to, or when it has had nothing to do for a while.
(import "././task/jobs.inc")
(import "./diagram.inc")

(defun main ()
	(job-child (task-timeout 10) (lambda (msg mbox)
		(defq said (list))
		(each (lambda ((kind file &rest args))
				(catch (case kind
						(:class (diagram-save (apply (const dia-class) args) file))
						(:tree (diagram-save (apply (const dia-hierarchy) args) file))
						(:script (repl (file-stream file) file)))
					(progn (push said (cat "Diagram " file ": " (str _))) :t)))
			(first (read (string-stream (slice msg +job_size -1)))))
		(mail-send (getf msg +job_reply)
			(apply (const cat) (cat (list (setf-> (str-alloc +job_reply_size)
				(+job_reply_key (getf msg +job_key)))) said))))))
