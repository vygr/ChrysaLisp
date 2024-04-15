;;;;;;;;;;;;;;
; file actions
;;;;;;;;;;;;;;

(defun trim-buffer (buffer)
	(when (> (bind '(w h) (. buffer :get_size)) 0)
		(undoable
			(bind '(cx cy) (. buffer :get_cursor))
			;trim front
			(defq y (some! 0 h :nil
					(# (unless (blank-line? %0) _))
				(list (. buffer :get_text_lines))))
			(unless y (setq y h))
			(.-> buffer (:set_cursor 0 y) (:cut 0 0))
			;trim end
			(defq y (some! (bind '(w h) (. buffer :get_size)) 0 :nil
					(# (unless (blank-line? %0) (inc _)))
				(list (. buffer :get_text_lines))))
			(unless y (setq y 0))
			(.-> buffer (:set_cursor 0 y) (:cut 0 h))
			;trim lines
			(each! 0 (bind '(w h) (. buffer :get_size)) (lambda (line)
					(unless (eql (defq trimed_line (trim-end line)) line)
						(.-> buffer
							(:set_cursor (length trimed_line) _)
							(:delete (- (length line) (length trimed_line))))))
				(list (. buffer :get_text_lines)))
			(bind '(cx cy) (. buffer :constrain cx cy))
			(. buffer :set_cursor cx cy)))
	buffer)

(defun action-trim ()
	(trim-buffer (. *edit* :get_buffer))
	(. *edit* :clear_selection)
	(refresh))

(defun save-file (file)
	(when (and file (defq meta (.-> *meta_map* (:find :files) (:find file))))
		(bind '(buffer) (gather meta :buffer))
		(when (. (trim-buffer buffer) :get_modified)
			(. buffer :file_save file))))

(defun action-save ()
	(save-file *current_file*)
	(state-save))

(defun action-save-all ()
	(each (# (save-file %0)) *open_files*)
	(state-save))

(defun action-new ()
	(when (not (eql (defq file (trim (get :clear_text *name_text*))) ""))
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
	(when (selection?)
		(bind '(y y1) (select-lines))
		(open-files (filter-array (# (/= (age %0) 0))
			(unique (sort (const cmp)
				(split (.-> (. *edit* :get_buffer)
					(:set_cursor 0 y1)
					(:copy 0 y)) (ascii-char +char_lf))))))))

(defun action-load-depends ()
	(action-save-all)
	(open-files (file-depends *current_file*)))

(defun action-load-all-depends ()
	(action-save-all)
	(open-files (all-file-depends (list *current_file*))))
