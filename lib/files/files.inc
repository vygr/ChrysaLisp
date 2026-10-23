;;;;;;;;;;;;;;;;;
; directory utils
;;;;;;;;;;;;;;;;;

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
					((eql type "8")
						;file
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

(defun files-depends (file &optional end)
	; (files-depends path [end]) -> paths
	;create list of immediate dependencies
	(defq files (list))
	(when (defq stream (file-stream file))
		(lines! (lambda (line)
			(and (eql "(" (first line))
				(or (starts-with "(include \q" line) (starts-with "(import \q" line))
				(push files (path-to-absolute
					(slice line (defq i (inc (find "\q" line))) (find "\q" line i)) file)))
			:nil)
			stream 0 (ifn end +max_long))
		(setq stream :nil))
	files)

(defun files-all-depends (files &optional imps end)
	; (files-all-depends paths [imps end]) -> paths
	;create list of all dependencies, with implicit options
	(each-mergeable (# (task-slice) (each (lambda (&rest %0) (merge files %0)) (files-depends %0 end)))
		(setq files (merge (merge (usort (cat files)) (ifn imps '()))
			'("class/lisp/root.inc" "class/lisp/task.inc")))) files)

;module
(export-symbols '(files-all files-dirs files-all-depends files-depends))
(env-pop)
