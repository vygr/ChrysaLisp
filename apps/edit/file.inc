;;;;;;;;;;;;;;
; file actions
;;;;;;;;;;;;;;

(defun trim-buffer (buffer)
	(when (> (bind '(w h) (. buffer :get_size)) 0)
		(undoable buffer
			(bind '(cx cy) (. buffer :get_cursor))
			;trim front
			(defq y (some! 0 h nil
					(# (unless (blank-line? %0) _))
				(list (. buffer :get_text_lines))))
			(unless y (setq y h))
			(.-> buffer (:set_cursor 0 y) (:cut 0 0))
			;trim end
			(defq y (some! (bind '(w h) (. buffer :get_size)) 0 nil
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

(defun action-save ()
	(save-open-files)
	(defq buffer (. *edit* :get_buffer))
	(when (and *current_file* (. (trim-buffer buffer) :get_modified))
		(. buffer :file_save *current_file*)
		(. *edit* :clear_selection)
		(refresh)))

(defun action-save-all ()
	(save-open-files)
	(. *meta_map* :each (lambda (file (_ _ _ _ _ _ _ buffer))
		(if (and file (. (trim-buffer buffer) :get_modified))
			(. buffer :file_save file))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-new ()
	(when (not (eql (defq file (get :clear_text *name_text*)) ""))
		(if (= (age file) 0)
			;file does not exist
			(save "" file))
		(def *name_text* :cursor 0 :anchor 0 :clear_text "")
		(.-> *name_text* :layout :dirty_all)
		(. *open_tree* :add_route file)
		(. *file_tree* :add_route file)
		(select-node file)
		(populate-vdu file)))

(defun action-next-buffer ()
	(defq live_buffers (list))
	(. *meta_map* :each (lambda (k v) (push live_buffers k)))
	(defq idx (inc (find *current_file* live_buffers)))
	(if (>= idx (length live_buffers)) (setq idx 0))
	(select-node (elem idx live_buffers))
	(populate-vdu (elem idx live_buffers)))

(defun action-prev-buffer ()
	(defq live_buffers (list))
	(. *meta_map* :each (lambda (k v) (push live_buffers k)))
	(defq idx (dec (find *current_file* live_buffers)))
	(if (< idx 0) (setq idx (dec (length live_buffers))))
	(select-node (elem idx live_buffers))
	(populate-vdu (elem idx live_buffers)))

(defun action-scratch-buffer ()
	(select-node nil)
	(populate-vdu nil))

(defun action-close-buffer ()
	(when *current_file*
		(each (# (. %0 :sub)) (. *open_tree* :children))
		(. *meta_map* :erase *current_file*)
		(when (defq idx (find *current_file* *open_files*))
			(setq *open_files* (erase *open_files* idx (inc idx))))
		(populate-open-tree)
		(populate-vdu nil)
		(select-node nil)))
