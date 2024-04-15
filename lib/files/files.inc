;;;;;;;;;;;;;;;;;
; directory utils
;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun all-files (&optional root exts cut_start cut_end)
	; (all-files [root exts cut_start cut_end]) -> paths
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

(defun all-dirs (paths)
	; (all-dirs paths) -> paths
	;return all the dir paths
	(reduce (lambda (dirs file)
		(defq dir (find-rev "/" file) dir (if dir (cat (slice file 0 dir) "/.")))
		(if (and dir (not (find dir dirs)))
			(push dirs dir) dirs)) paths (list)))

(defun file-depends (file)
	; (file-depends path) -> paths
	;create list of immediate dependencies
	(defq files (list file))
	(each-line (lambda (line)
		(when (and (> (length line) 8) (eql "(" (first line))
				(<= 2 (length (defq s (split line (const (unescape " '()\q\r\t"))))) 4))
			(cond
				((eql (first s) "include")
					(push files (abs-path (second s) file)))
				((eql (first s) "import")
					(push files (abs-path (second s) file))))))
		(if (= (age file) 0)
			(throw "No such file !" file)
			(file-stream file))) files)

(defun all-file-depends (files &optional imp)
	; (all-file-depends paths &optional imp) -> paths
	;create list of all dependencies, with implicte option
	(setq files (unique (sort (const cmp) (cat files))))
	(when imp
		(each (# (unless (find %0 files) (push files %0)))
			(list "class/lisp/root.inc" "class/lisp/task.inc")))
	(each-mergeable (lambda (file)
		(each (# (unless (find %0 files) (push files %0)))
			(file-depends file))) files))

;module
(export-symbols '(all-files all-dirs all-file-depends file-depends))
(env-pop)
