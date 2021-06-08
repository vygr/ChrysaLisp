;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(radio-select find_toolbar (if *whole_words* 2 -1)))

(defun filter-whole-words (found pattern)
	(when *whole_words*
		(defq pattern_len (length pattern) found
			(filter (lambda ((x y))
				(defq line (. *current_buffer* :get_text_line y) x1 (+ x pattern_len))
				(cond
					((eql pattern line))
					((= x 0) (not-whole-word-char? (elem x1 line)))
					((= x1 (length line)) (not-whole-word-char? (elem (dec x) line)))
					((and (not-whole-word-char? (elem (dec x) line)))
						(not-whole-word-char? (elem x1 line)))))
				found))) found)

(defun action-find-down ()
	(unless (eql (defq pattern (get :clear_text *find_text*)) "")
		(when (> (length (defq found (filter-whole-words (. *current_buffer* :find pattern) pattern))) 0)
			(when (defq next (some (# (bind '(x y) %0)
					(if (or (> y *cursor_y*)
							(and (= y *cursor_y*) (>= x *cursor_x*)))
						%0)) found))
				(bind '(x y) next)
				(setq *anchor_x* x *anchor_y* y)
				(. *current_buffer* :set_cursor (+ x (length pattern)) y)
				(refresh)))))

(defun action-find-up ()
	(unless (eql (defq pattern (get :clear_text *find_text*)) "")
		(when (> (length (defq found (filter-whole-words (. *current_buffer* :find pattern) pattern))) 0)
			(when (defq next (some-rev (# (bind '(x y) %0)
					(if (or (< y *cursor_y*)
							(and (= y *cursor_y*) (< x *cursor_x*)))
						%0)) found))
				(bind '(x y) next)
				(setq *anchor_x* (+ x (length pattern)) *anchor_y* y)
				(. *current_buffer* :set_cursor x y)
				(refresh)))))

(defun action-set-find-text ()
	(if (selection?)
		(when (> (length (defq text
				(split (. *current_buffer* :copy *anchor_x* *anchor_y*)
					(ascii-char +char_lf)))) 0)
			(setq text (elem 0 text))
			(set *find_text* :clear_text text :cursor (length text))
			(.-> *find_text* :layout :dirty))))

(defun action-replace ()
	(unless (or (eql (defq pattern (get :clear_text *find_text*)) "")
				(eql (defq replace (get :clear_text *replace_text*)) pattern))
		(when (eql (. *current_buffer* :copy *anchor_x* *anchor_y*) pattern)
			(undoable
				(. buffer :cut *anchor_x* *anchor_y*)
				(. buffer :insert replace))
			(refresh))
		(action-find-down)))

(defun action-replace-all ()
	(unless (or (eql (defq pattern (get :clear_text *find_text*)) "")
				(eql (defq replace (get :clear_text *replace_text*)) pattern))
		(when (> (length (defq found (filter-whole-words (. *current_buffer* :find pattern) pattern))) 0)
			(undoable
				(each-rev (lambda ((x y))
						(.-> buffer (:set_cursor x y) (:cut (+ x (length pattern)) y))
						(unless (eql replace "") (. buffer :insert replace)))
					found))
			(clear-selection) (refresh))))
