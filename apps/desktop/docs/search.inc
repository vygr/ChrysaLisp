;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun search-key (&rest _) (str _))

(defun find-global (pattern whole_words regexp ignore_case)
	;find file matches
	(defq cmd (cat "grep -c -f -m "
			(if whole_words "-w " "") (if regexp "-x " "") (if ignore_case "-i " "")
			(hex-encode pattern) " ")
		results (sort (map (# (trim-end %0 "\n"))
			(filter nempty? (map (const second)
				(pipe-farm (map (# (cat cmd %0))
					(files-all "docs" '(".md"))) 20000000)))))))

(defun show-matches (files)
	;show file matches
	(each (# (. *file_selector* :highlight %0)) files))

(defun clear-matches (files)
	;clear file matches
	(each (# (. *file_selector* :highlight %0 :t)) files))

(defun refresh-matches ()
	(defq search_key (search-key
		(defq pattern (. *find_text* :get_text)) *whole_words* *regexp* *ignore_case*))
	(unless (eql search_key *last_key*)
		(clear-matches *last_files*)
		(setq *last_files* '() *last_key* search_key)
		(unless (eql pattern "")
			(show-matches (setq *last_files*
				(find-global pattern *whole_words* *regexp* *ignore_case*))))))

(defun find-update (&optional step)
	(setd step 0)
	(refresh-matches)
	(bind '(search meta pattern) (query (. *find_text* :get_text) *whole_words* *regexp* *ignore_case*))
	(defq found (if (and meta (nql pattern ""))
		(filter (# (. search :match? (if *ignore_case* (to-lower (get :text %0)) (get :text %0)) meta)) *search_widgets*))
		cur_match (and *last_widget*
			(some (# (if (= (. %0 :get_id) (. *last_widget* :get_id)) (!))) found)))
	(if *last_widget*
		(def (. *last_widget* :dirty) :color 0))
	(if (nempty? found)
		(progn
			(setq *last_widget* (cond
				((and cur_match (= step 0))
					(elem-get found cur_match))
				(cur_match
					(elem-get found (% (+ cur_match step (length found)) (length found))))
				((if (< step 0) (last found) (first found)))))
			(def (. *last_widget* :dirty) :color *env_highlight_col*)
			(. *page_scroll* :visible *last_widget*))
		(setq *last_widget* :nil)))

(defun action-find-down () (find-update 1))
(defun action-find-up () (find-update -1))

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* *ignore_case* :nil :nil))
	(find-update 0))

(defun action-regexp ()
	(setq *regexp* (not *regexp*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* *ignore_case* :nil :nil))
	(find-update 0))

(defun action-ignore-case ()
	(setq *ignore_case* (not *ignore_case*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* *ignore_case* :nil :nil))
	(find-update 0))

(defun action-next-doc ()
	(refresh-matches)
	(when (nempty? *last_files*)
		(if (defq idx (find *current_file* *last_files*))
			(setq *current_file* (elem-get
				*last_files* (% (inc idx) (length *last_files*))))
			(setq *current_file* (first *last_files*)))
		(populate-page *current_file*)
		(. *file_selector* :select_node *current_file*)
		(action-find-down)))

(defun action-prev-doc ()
	(refresh-matches)
	(when (nempty? *last_files*)
		(if (defq idx (find *current_file* *last_files*))
			(setq *current_file* (elem-get
				*last_files* (% (+ idx (length *last_files*) -1) (length *last_files*))))
			(setq *current_file* (first *last_files*)))
		(populate-page *current_file*)
		(. *file_selector* :select_node *current_file*)
		(action-find-down)))