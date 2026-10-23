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

(defun source-bundle (path)
	; (source-bundle path) -> :nil | text
	;of obj/vp/<mode>/<function>, the bundle of the VP the function is in,
	;one of the bundles of its folder. The VP is what was built, not secret
	(defq parts (split path "/") mode (elem-get parts 2) name (join (slice parts 3 -1) "/")
		dir (cat "obj/vp/" mode "/" (path-dir name) "/"))
	(and (find mode '("release" "debug" "validate")) (not (find ".." parts))
		(some (lambda (file kind)
				(and (eql kind "8") (ends-with ".vp" file)
					(defq text (load (cat dir file)) stream (string-stream text))
					(progn (while (and (defq entry (first (read stream))) (nql (first entry) name)))
						(if entry text))))
			(first (defq listed (dir-list dir))) (second listed))))

(defun source-translator ()
	; (source-translator)
	;the translator for this CPU, made the first time it is wanted and kept:
	;the op and register lists of the VP, the translator, this CPU's emitter
	(unless (get 'emit-translate *source_env*)
		(bind '(& cpu abi) (map (const sym) (split (load-path) "/")))
		(def *source_env* '*cpu* cpu '*abi* abi '*build_mode* (if (eql (source-mode) "debug") 1 0)
			'validatecase (macro (&rest e) (if (>= *build_mode* 2) `(progn ~e))))
		(each (# (import %0 *source_env*)) '("lib/asm/regs.inc" "lib/asm/ops.inc" "lib/trans/vp.inc"))))

(defun source-translate (args)
	; (source-translate request) -> :t | :nil
	;the request is the system id, its @Source in hex, the build mode and
	;the function, with a space between each. A native of another system
	;made ready for this one's tasks of it: the
	;bundle it is in asked of that system's @Source, each function of it
	;translated into source/<sid>/ under the load path, then each it links
	;to that this node has not built, a link seen once so a ring of them
	;ends. One at a time, so a function asked for twice is made once
	(bind '(sid svc mode name) (split args " "))
	(defq svc (hex-decode svc) folder (cat (load-path) "source/" sid "/") todo (list name) seen (list) ok :t)
	(while (defq name (pop todo))
		(unless (or (find name seen) (/= (age (cat (load-path) name)) 0) (/= (age (cat folder name)) 0))
			(push seen name)
			(cond
				;of its own system's VP, as a test is, it reads it, a service can
				;not answer itself while it waits
				((defq bundle (if (eql svc (task-mbox)) (source-bundle (cat "obj/vp/" mode "/" name))
						(source-ask svc (cat "obj/vp/" mode "/" name))))
					(source-translator)
					(defq stream (string-stream bundle))
					(while (defq entry (first (read stream)))
						(defq out ((get 'emit-translate *source_env*) (second entry)))
						(save out (cat folder (first entry)))
						(each (# (push todo %0)) (source-links out))))
				((setq ok :nil)))))
	ok)

(defun main ()
	(defq service (mail-declare (task-mbox) "@Source" "Source Service 0.1") *source_env* (env))
	(while :t
		(defq msg (mail-read (task-mbox)) path (slice msg +source_rpc_size -1))
		(mail-send (getf msg +source_rpc_reply_id)
			(cond
				((starts-with "+" path)
					(if (source-translate (slice path 1 -1)) "y" "n"))
				((starts-with "obj/vp/" path)
					(if (defq text (source-bundle path)) (cat "y" text) "n"))
				((and (source-given? path) (defq text (load path))) (cat "y" text))
				("n")))))
