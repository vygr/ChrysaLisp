;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

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

(defun refresh-matches (pattern)
	(clear-matches *last_files*)
	(setq *last_files* '())
	(unless (eql pattern "")
		(show-matches (setq
			*last_pattern* pattern
			*last_files* (find-global pattern)))))

(defun action-find-down ()
	(unless (eql (defq pattern (. *find_text* :get_text)) *last_pattern*)
		(refresh-matches pattern))
	(when (nempty? *last_files*)
		(if (defq idx (find *current_file* *last_files*))
			(setq *current_file* (elem-get
				(% (inc idx) (length *last_files*)) *last_files*))
			(setq *current_file* (first *last_files*)))
		(populate-page *current_file*)
		(visible-node *file_tree* *current_file*)))

(defun action-find-up ()
	(unless (eql (defq pattern (. *find_text* :get_text)) *last_pattern*)
		(refresh-matches pattern))
	(when (nempty? *last_files*)
		(if (defq idx (find *current_file* *last_files*))
			(setq *current_file* (elem-get
				(% (+ idx (length *last_files*) -1) (length *last_files*)) *last_files*))
			(setq *current_file* (first *last_files*)))
		(populate-page *current_file*)
		(visible-node *file_tree* *current_file*)))

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* :nil :nil))
	(refresh-matches (. *find_text* :get_text)))

(defun action-regexp ()
	(setq *regexp* (not *regexp*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* :nil :nil))
	(refresh-matches (. *find_text* :get_text)))
