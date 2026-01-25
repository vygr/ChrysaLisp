;;;;;;;;;;;;;;
; file actions
;;;;;;;;;;;;;;

(defun save-file (file)
	(when (and file (defq meta (.-> *meta_map* (:find :files) (:find file))))
		(bind '(buffer) (gather meta :buffer))
		(when (.-> buffer :trim :get_modified)
			(. buffer :file_save file))))

(defun action-save ()
	(save-file *current_file*)
	(state-save))

(defun action-save-all ()
	(each (# (save-file %0)) *open_files*)
	(state-save))

(defun action-new ()
	(when (nql (defq file (trim (get :clear_text *name_text*))) "")
		;file does not exist ?
		(if (= (age file) 0) (save "" file))
		(def *name_text* :cursor 0 :anchor 0 :clear_text "")
		(.-> *name_text* :layout :dirty_all)
		(open-file file)))

(defun action-next-buffer ()
	(defq files (cat *open_files* '(:nil))
		idx (% (inc (find *current_file* files)) (length files)))
	(unless (elem-get files idx) (setq idx (% (inc idx) (length files))))
	(switch-file (elem-get files idx)))

(defun action-prev-buffer ()
	(defq files (cat *open_files* '(:nil))
		idx (% (+ (dec (find *current_file* files)) (length files)) (length files)))
	(unless (elem-get files idx) (setq idx (% (+ (dec idx) (length files)) (length files))))
	(switch-file (elem-get files idx)))

(defun action-scratch-buffer ()
	(populate-vdu :nil)
	(select-node :nil))

(defun close-file (file)
	(save-file file)
	(when (and file (defq i (find file *open_files*)))
		(.-> *meta_map* (:find :files) (:erase file))
		(setq *open_files* (erase *open_files* i (inc i)))
		(defq files (cat *open_files*))
		(if (= (length files) 0) (push files :nil))
		(elem-get files (% i (length files)))))

(defun action-close-buffer ()
	(defq file (close-file *current_file*))
	(populate-file-trees)
	(switch-file file))

(defun action-close-all ()
	(each (# (close-file %0)) (cat *open_files*))
	(populate-file-trees)
	(switch-file :nil))

(defun action-load-selected ()
	(defq lines (list) buffer (. *edit* :get_buffer))
	(each (lambda (csr)
			(bind '(& sy1 & sy &ignore) (csr-floor csr))
			(until (= sy sy1)
				(merge lines (list (most (. buffer :get_text_line sy))))
				(++ sy)))
		(. buffer :get_cursors))
	(open-files (filter (# (/= (age %0) 0)) (usort lines))))

(defun action-load-depends ()
	(when *current_file*
		(action-save-all)
		(open-files (files-depends *current_file*))))

(defun action-load-all-depends ()
	(when *current_file*
		(action-save-all)
		(open-files (files-all-depends (list *current_file*)))))
