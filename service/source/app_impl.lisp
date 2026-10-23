(import "./app.inc")

(defun source-given? (path)
	; (source-given? path) -> :t | :nil
	;a file that can be imported, Lisp, by a path from the root of the tree:
	;none above it, none in a user's own folder, usr/env.inc is the
	;system's. Nothing else of the tree is given, its keys are in it
	(defq parts (split path "/"))
	(and (or (ends-with ".lisp" path) (ends-with ".inc" path))
		(not (or (starts-with "/" path) (find ".." parts)
			(and (eql (first parts) "usr") (> (length parts) 2))))))

(defun main ()
	(defq service (mail-declare (task-mbox) "@Source" "Source Service 0.1"))
	(while :t
		(defq msg (mail-read (task-mbox)) path (slice msg +source_rpc_size -1))
		(mail-send (getf msg +source_rpc_reply_id)
			(if (and (source-given? path) (defq text (load path))) (cat "y" text) "n"))))
