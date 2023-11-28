;;;;;;;
; utils
;;;;;;;

(defmacro not-whole-word-char? (_)
	`(bfind ,_ +char_class_not_whole_word))

(defun blank-line? (line)
	(every (# (eql " " %0)) line))

(defmacro undoable (&rest _)
	`(progn
		(bind '(cx cy) (. *edit* :get_cursor))
		(. buffer :push_undo
			(list :mark (defq mark (. buffer :next_mark)))
			(list :cursor cx cy))
		~_
		(. buffer :push_undo (list :mark mark))))

(defun selection? ()
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(or (/= ax cx) (/= ay cy)))

(defun sort-selection ()
	(bind '(sx sy) (. *edit* :get_cursor))
	(bind '(sx1 sy1) (. *edit* :get_anchor))
	(if (> sy sy1) (defq st sx sx sx1 sx1 st st sy sy sy1 sy1 st))
	(and (= sy sy1) (> sx sx1) (defq st sx sx sx1 sx1 st))
	(list sx sy sx1 sy1))

(defun select-lines ()
	(bind '(sx sy sx1 sy1) (sort-selection))
	(if (or (> sx1 0) (= sy sy1)) (setq sy1 (inc sy1)))
	(list sy sy1))

(defun select-word ()
	(bind '(cx cy) (. *edit* :get_cursor))
	(defq line (.-> *edit* :get_buffer (:get_text_line cy))
		x (some! cx 0 :nil (#
			(if (not-whole-word-char? %0) (inc _))) (list line))
		x1 (some! cx -1 :nil (#
		   (if (not-whole-word-char? %0) _)) (list line)))
	(unless x (setq x 0))
	(unless x1 (setq x1 (length line)))
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
	(radio-select *find_toolbar*
		(list :nil (> fy1 fy) *whole_words* *regexp* :nil :nil)))

(defq +range_0_10 `'(~(range 0 10)))
