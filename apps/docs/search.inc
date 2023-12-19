;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* :nil :nil)))

(defun action-regexp ()
	(setq *regexp* (not *regexp*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* :nil :nil)))

(defun find-global (pattern)
	;find file matches
	(defq cmd (cat "grep -c -f "
			(if *whole_words* "-w " "") (if *regexp* "-r " "")
			(id-encode pattern) " ")
		results (sort (const cmp) (map (# (trim-end %0 (ascii-char 10)))
			(filter nempty? (map (const second)
				(pipe-farm (map (# (cat cmd %0))
					(all-files "docs" '(".md"))) 20000000)))))))

(defun show-matches (files)
	;show file matches
	(each (# (. *file_tree* :highlight %0)) files))

(defun clear-matches (files)
	;clear file matches
	(each (# (. *file_tree* :highlight %0 :t)) files))

(defun action-find-up ()
	(clear-matches *last_files*)
	(setq *last_files* '())
	(unless (eql (defq pattern (. *find_text* :get_text)) "")
		(show-matches (setq
			*last_pattern* pattern
			*last_files* (find-global pattern)))))

(defun action-find-down ()
	(clear-matches *last_files*)
	(setq *last_files* '())
	(unless (eql (defq pattern (. *find_text* :get_text)) "")
		(show-matches (setq
			*last_pattern* pattern
			*last_files* (find-global pattern)))))
