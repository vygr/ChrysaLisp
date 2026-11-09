;;;;;;;;;;;;;;;;;
; directory utils
;;;;;;;;;;;;;;;;;

(import "././text/tree.inc")

;module
(env-push)

(defun files-all (&optional root exts cut_start cut_end)
	; (files-all [root exts cut_start cut_end]) -> paths
	;all source files from root downwards, none recursive
	(setd root "." exts '("") cut_start 0 cut_end -1)
	;a root given with a / on its end, apps/, is the same folder: each path
	;is joined to it with a / of its own, and had two
	(and (> (length root) 1) (ends-with "/" root) (setq root (slice root 0 -2)))
	(defq stack (list root) paths (list))
	(while (setq root (pop stack))
		;not what is built, nor this machine's store
		(unless (or (starts-with "./obj" root) (starts-with "./storage" root))
			(each! (lambda (file type)
					(cond
						((or (eql type "f") (eql type "l"))
							;file, or a link to one
							(if (some (# (ends-with %0 file)) exts)
								(push paths (cat root "/" file))))
						(:t ;dir
							(unless (starts-with "." file)
								(push stack (cat root "/" file))))))
				(dir-list root))))
	(if (or (/= 0 cut_start) (/= -1 cut_end))
		(map (# (slice %0 cut_start cut_end)) paths)
		paths))

(defun files-dirs (paths)
	; (files-dirs paths) -> paths
	;return all the dir paths
	(reduce (lambda (dirs file)
			(defq dir (rfind "/" file) dir (if dir (cat (slice file 0 dir) ".")))
			(if (and dir (not (find dir dirs)))
				(push dirs dir) dirs)) paths (list)))

(defq +scan_kinds ''("import" "include" "def-func" "def-method"
		"gen-host-call" "gen-vtable" "gen-type" "gen-create"))

(defun scan-kids (node)
	; (scan-kids node) -> nodes
	;what is in a form, its comments and empty lines left out
	(filter (lambda (kid) (not (find (first kid) '(:comment :blank)))) (third node)))

(defun scan-name (node file)
	; (scan-name node path) -> str
	;a name a form is given: a string without its quotes, a quoted symbol
	;without its quote, the path of a (path-to-absolute "name")
	(defq abs (eql (first node) :form))
	(if abs (setq node (second (scan-kids node))))
	(defq text (third node))
	(case (first node)
		((:string1 :string2) (setq text (slice text 1 -2)))
		(:symbol (if (starts-with "'" text) (setq text (slice text 1 -1)))))
	(if abs (path-to-absolute text file) text))

(defun files-scan (file &optional end heads)
	; (files-scan path [end heads]) -> ((kind line name ...) ...)
	;what a file imports and includes, and the functions it makes, read as
	;the highlighter reads it, (tree-read) of these heads: each form at the
	;top of it that is an (import) (include) (def-func) (def-method)
	;(gen-host-call) (gen-vtable) (gen-type) or (gen-create), as its kind,
	;the line it is on and the first two names it is given, (scan-name).
	;Such a form begins its line, as the source is laid out, check -c scan
	;says where one does not. One inside another form is made as the code
	;runs, it is not of the file. Only the first end lines, if given, and
	;the forms of other heads, if given, ("def-class" "defun") say
	(setd heads +scan_kinds)
	(defq out (list))
	(when (defq stream (file-stream file))
		(each (lambda (node)
				(and (eql (first node) :form)
					(> (length (defq kids (scan-kids node))) 1)
					(eql (first (first kids)) :symbol)
					(find (defq kind (third (first kids))) heads)
					;a path worked out as the code runs is not read here
					(or (not (find kind '("import" "include")))
						(find (first (second kids)) '(:string1 :string2)))
					(push out (reduce! (lambda (entry kid) (push entry (scan-name kid file)))
							(list kids) (list kind (second node)) 1 (min 3 (length kids))))))
			(first (tree-read stream end heads)))
		(setq stream :nil))
	out)

(defun files-depends (file &optional end)
	; (files-depends path [end]) -> paths
	;create list of immediate dependencies, what it imports and includes
	(reduce (lambda (files (kind & name &ignore))
			(if (find kind '("import" "include")) (push files (path-to-absolute name file)) files))
		(files-scan file end) (list)))

(defun files-all-depends (files &optional imps end)
	; (files-all-depends paths [imps end]) -> paths
	;create list of all dependencies, with implicit options
	(each-mergeable (# (task-slice) (each (lambda (&rest %0) (merge files %0)) (files-depends %0 end)))
		(setq files (merge (merge (usort (cat files)) (ifn imps '()))
				'("class/lisp/root.inc" "class/lisp/task.inc")))) files)

;module
(export-symbols '(files-all files-dirs files-all-depends files-depends files-scan +scan_kinds))
(env-pop)
