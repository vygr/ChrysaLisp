;;;;;;;;;;;;;;;;;
; directory utils
;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun files-all (&optional root exts cut_start cut_end)
	; (files-all [root exts cut_start cut_end]) -> paths
	;all source files from root downwards, none recursive
	(setd root "." exts '("") cut_start 0 cut_end -1)
	(defq stack (list root) paths (list))
	(while (setq root (pop stack))
		(unless (starts-with "./obj" root)
			(each! 0 -1 (lambda (file type)
				(cond
					((eql type "8")
						;file
						(if (some (# (ends-with %0 file)) exts)
							(push paths (cat root "/" file))))
					(:t ;dir
						(unless (starts-with "." file)
							(push stack (cat root "/" file))))))
				(unzip (split (pii-dirlist root) ",") 2))))
	(if (or (/= 0 cut_start) (/= -1 cut_end))
		(map (# (slice %0 cut_start cut_end)) paths)
		paths))

(defun files-dirs (paths)
	; (files-dirs paths) -> paths
	;return all the dir paths
	(reduce (lambda (dirs file)
		(defq dir (find-rev "/" file) dir (if dir (cat (slice file 0 dir) "/.")))
		(if (and dir (not (find dir dirs)))
			(push dirs dir) dirs)) paths (list)))

(defun files-depends (file)
	; (files-depends path) -> paths
	;create list of immediate dependencies
	(defq files (list file))
	(each-line (lambda (line)
		(and (> (length line) 8) (eql "(" (first line))
			(= (length (defq s (split line (const (char-class " ()\q\r\t"))))) 2)
			(find (first s) '("include" "import"))
			(push files (abs-path (second s) file))))
		(if (= (age file) 0)
			(throw "No such file !" file)
			(file-stream file))) files)

(defun files-all-depends (files &optional imps)
	; (files-all-depends paths &optional imps) -> paths
	;create list of all dependencies, with implicit options
	(setq files (usort (cat files)))
	(when imps
		(each (# (unless (find %0 files) (push files %0)))
			(cat (list "class/lisp/root.inc" "class/lisp/task.inc") imps)))
	(each-mergeable (lambda (file)
		(each (# (unless (find %0 files) (push files %0)))
			(files-depends file))) files))

;module
(export-symbols '(files-all files-dirs files-all-depends files-depends))
(env-pop)
