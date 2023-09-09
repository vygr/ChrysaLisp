;;;;;;;;;;;;;;
; file actions
;;;;;;;;;;;;;;

(defun trim-buffer (buffer)
	(when (> (bind '(w h) (. buffer :get_size)) 0)
		(undoable buffer
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
	(when (and file (defq state (. *meta_map* :find file)))
		(bind '(_ _ _ _ _ _ _ buffer) state)
		(when (. (trim-buffer buffer) :get_modified)
			(. buffer :file_save file))))

(defun action-save ()
	(save-file *current_file*)
	(save-state))

(defun action-save-all ()
	(each (# (save-file %0)) *open_files*)
	(save-state))

(defun action-new ()
	(when (not (eql (defq file (get :clear_text *name_text*)) ""))
		;file does not exist ?
		(if (= (age file) 0) (save "" file))
		(def *name_text* :cursor 0 :anchor 0 :clear_text "")
		(.-> *name_text* :layout :dirty_all)
		(switch-file file)))

(defun action-next-buffer ()
	(defq files (cat *open_files* '(:nil))
		idx (% (inc (find *current_file* files)) (length files)))
	(unless (elem-get idx files) (setq idx (% (inc idx) (length files))))
	(switch-file (elem-get idx files)))

(defun action-prev-buffer ()
	(defq files (cat *open_files* '(:nil))
		idx (% (+ (dec (find *current_file* files)) (length files)) (length files)))
	(unless (elem-get idx files) (setq idx (% (+ (dec idx) (length files)) (length files))))
	(switch-file (elem-get idx files)))

(defun action-scratch-buffer ()
	(select-node :nil)
	(populate-vdu :nil))

(defun close-file (file)
	(save-file file)
	(when (and file (defq i (find file *open_files*)))
		(. *meta_map* :erase file)
		(setq *open_files* (erase *open_files* i (inc i)))
		(defq files (cat *open_files*))
		(if (= (length files) 0) (push files :nil))
		(elem-get (% i (length files)) files)))

(defun action-close-buffer ()
	(defq file (close-file *current_file*))
	(populate-open-tree)
	(select-node file)
	(populate-vdu file)
	(save-state))

(defun action-close-all ()
	(each (# (close-file %0)) (cat *open_files*))
	(switch-file :nil))
