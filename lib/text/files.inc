;;;;;;;;;;;;;;;;;;;;;;
; directory text utils
;;;;;;;;;;;;;;;;;;;;;;

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
				(unzip (split-str (pii-dirlist root) ",") 2))))
	(if (or (/= 0 cut_start) (/= -1 cut_end))
		(map (# (slice cut_start cut_end %0)) paths)
		paths))

(defun all-dirs (paths)
	; (all-dirs paths) -> paths
	;return all the dir paths
	(reduce (lambda (dirs file)
		(defq dir (find-rev "/" file) dir (if dir (cat (slice 0 dir file) "/.")))
		(if (and dir (not (find dir dirs)))
			(push dirs dir) dirs)) paths (list)))

;module
(export-symbols '(all-files all-dirs))
(env-pop)
