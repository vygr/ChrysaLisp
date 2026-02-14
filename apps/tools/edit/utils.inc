;;;;;;;
; utils
;;;;;;;

(defun select-word ()
	(bind '(cx cy &ignore) (. *edit* :get_cursor))
	(defq line (.-> *edit* :get_buffer (:get_text_line cy))
		x (rbskipn +char_class_not_whole_word line cx)
		x1 (bskipn +char_class_not_whole_word line cx))
	(list x x1))

(defun switch-file (file)
	;switch to file or scratch (:nil)
	(populate-vdu file)
	(select-node file))

(defun open-file (file)
	;open file or scratch (:nil)
	(and file (not (find file *open_files*))
		(push *open_files* file))
	(populate-file-trees)
	(switch-file file))

(defun open-files (files)
	(each (# (unless (find %0 *open_files*)
		(push *open_files* %0)
		(populate-buffer %0 0 0 0 0 0 0))) files)
	(populate-file-trees)
	(select-node *current_file*))

(defun eof-gaps ()
	(list (- (length (. buffer :get_text_line cy)) cx)
		(- (second (. buffer :get_size)) cy)))

(defmacro for-all-buffers (&rest _)
	`(progn
		(unless *current_file* (action-next-buffer))
		(when (defq *exit* :nil *here* *current_file*)
			(push *refresh_mode* 1)
			(until *exit*
				~_
				(update-meta-data)
				(action-next-buffer)
				(setq *exit* (eql *current_file* *here*)))
			(pop *refresh_mode*)
			(refresh))))

(defun update-find-toolbar ()
	(defq focus (. *edit* :get_focus)
		state (nql focus +invalid_focus))
	(toolbar-states *find_toolbar*
		(if (= (length (. *find_toolbar* :children)) 6)
			(list :nil state *whole_words* *regexp* :nil :nil)
			(list state *whole_words* *regexp* :nil :nil))))

(defun find-count ()
	(defq buffer (. *edit* :get_buffer)
		found (. buffer :get_buffer_found) fc 0
		focus (. *edit* :get_focus)
		fy 0 fy1 (length found))
	(when (nql focus +invalid_focus)
		(bind '(& _fy & _fy1) focus)
		(if (< _fy _fy1) (setq fy _fy fy1 _fy1) (setq fy _fy1 fy1 _fy)))
	(each! (# (setq fc (+ fc (if %0 (length %0) 0)))) (list found) fy fy1)
	fc)

(defq +range_0_10 `'(~(range 0 10)))
