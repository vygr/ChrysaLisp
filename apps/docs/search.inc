;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun search-key (&rest _) (str _))

(defun find-global (pattern whole_words regexp)
	;find file matches
	(defq cmd (cat "grep -c -f -m "
			(if whole_words "-w " "") (if regexp "-r " "")
			(id-encode pattern) " ")
		results (sort (const cmp) (map (# (trim-end %0 (ascii-char 10)))
			(filter-array nempty? (map (const second)
				(pipe-farm (map (# (cat cmd %0))
					(all-files "docs" '(".md"))) 20000000)))))))

(defun show-matches (files)
	;show file matches
	(each (# (. *file_tree* :highlight %0)) files))

(defun clear-matches (files)
	;clear file matches
	(each (# (. *file_tree* :highlight %0 :t)) files))

(defun refresh-matches ()
	(defq search_key (search-key
		(defq pattern (. *find_text* :get_text)) *whole_words* *regexp*))
	(unless (eql search_key *last_key*)
		(clear-matches *last_files*)
		(setq *last_files* '() *last_key* search_key)
		(unless (eql pattern "")
			(show-matches (setq *last_files*
				(find-global pattern *whole_words* *regexp*))))))

(defun action-find-down ()
	(refresh-matches)
	(bind '(search pattern meta)
		(query (defq pattern (. *find_text* :get_text)) *whole_words* *regexp*))
	(unless (eql pattern "")
		(defq found (filter-array (# (. search :match? (get :text %0) pattern meta)) *search_widgets*))
		(when (nempty? found)
			(if *last_widget* (def (. *last_widget* :dirty) :color 0))
			(if (defq idx (find *last_widget* found))
				(setq *last_widget* (elem-get
					(% (inc idx) (length found)) found))
				(setq *last_widget* (first found)))
			(def (. *last_widget* :dirty) :color *env_highlight_col*)
			(. *page_scroll* :visible *last_widget*))))

(defun action-find-up ()
	(refresh-matches)
	(bind '(search pattern meta)
		(query (defq pattern (. *find_text* :get_text)) *whole_words* *regexp*))
	(unless (eql pattern "")
		(defq found (filter-array (# (. search :match? (get :text %0) pattern meta)) *search_widgets*))
		(when (nempty? found)
			(if *last_widget* (def (. *last_widget* :dirty) :color 0))
			(if (defq idx (find *last_widget* found))
				(setq *last_widget* (elem-get
					(% (+ idx (length found) -1) (length found)) found))
				(setq *last_widget* (first found)))
			(def (. *last_widget* :dirty) :color *env_highlight_col*)
			(. *page_scroll* :visible *last_widget*))))

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* :nil :nil))
	(refresh-matches))

(defun action-regexp ()
	(setq *regexp* (not *regexp*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* :nil :nil))
	(refresh-matches))

(defun action-next-doc ()
	(refresh-matches)
	(when (nempty? *last_files*)
		(if (defq idx (find *current_file* *last_files*))
			(setq *current_file* (elem-get
				(% (inc idx) (length *last_files*)) *last_files*))
			(setq *current_file* (first *last_files*)))
		(populate-page *current_file*)
		(visible-node *file_tree* *current_file*)
		(action-find-down)))

(defun action-prev-doc ()
	(refresh-matches)
	(when (nempty? *last_files*)
		(if (defq idx (find *current_file* *last_files*))
			(setq *current_file* (elem-get
				(% (+ idx (length *last_files*) -1) (length *last_files*)) *last_files*))
			(setq *current_file* (first *last_files*)))
		(populate-page *current_file*)
		(visible-node *file_tree* *current_file*)
		(action-find-down)))
