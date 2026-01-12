;;;;;;;
; utils
;;;;;;;

(defun blank-line? (line)
	(eql (rbskip +char_class_white_space line -1) 0))

(defmacro undoable (&rest _)
	`(progn
		(. buffer :push_undo
			(list :mark (defq mark (. buffer :next_mark))))
		~_
		(. buffer :push_undo (list :mark mark))))

(defun sort-selection ()
	(bind '(sx sy sx1 sy1) (. *edit* :get_cursor))
	(if (> sy sy1) (defq st sx sx sx1 sx1 st st sy sy sy1 sy1 st))
	(and (= sy sy1) (> sx sx1) (defq st sx sx sx1 sx1 st))
	(list sx sy sx1 sy1))

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
	(bind '(fx fy fx1 fy1) (. *edit* :get_find))
	(toolbar-states *find_toolbar*
		(list :nil (> fy1 fy) *whole_words* *regexp* :nil :nil)))

(defun find-count ()
	(defq buffer (. *edit* :get_buffer)
		found (. buffer :get_buffer_found) fc 0)
	(bind '(fx fy fx1 fy1) (. *edit* :get_find))
	(if (>= fy fy1) (setq fy 0 fy1 (length found)))
	(each! (# (setq fc (+ fc (if %0 (length %0) 0)))) (list found) fy fy1)
	fc)

(defq +range_0_10 `'(~(range 0 10)))
