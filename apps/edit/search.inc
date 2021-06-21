;;;;;;;;;;;;;;;;
; search actions
;;;;;;;;;;;;;;;;

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(radio-select find_toolbar (if *whole_words* 2 -1)))

(defun filter-whole-words (found pattern)
	(cond
		(*whole_words*
			(defq pattern_len (length pattern) out (cap (length found) (list)))
			(each (lambda (line xs)
					(defq out_xs (list))
					(each (lambda (x)
						(defq x1 (+ x pattern_len))
						(if (cond
								((eql pattern line))
								((= x 0) (not-whole-word-char? (elem x1 line)))
								((= x1 (length line)) (not-whole-word-char? (elem (dec x) line)))
								((and (not-whole-word-char? (elem (dec x) line)))
									(not-whole-word-char? (elem x1 line))))
							(push out_xs x))) xs)
					(push out out_xs))
				(. *current_buffer* :get_text_lines) found)
			  out)
		(found)))

(defun action-find-down ()
	(unless (eql (defq pattern (get :clear_text *find_text*)) "")
		(if (> (length (defq found (filter-whole-words (. *current_buffer* :find pattern) pattern))) 0)
			(when (defq next (some! *cursor_y* -1 nil (lambda (xs)
					(defq y _)
					(some (lambda (x) (cond
						((> y *cursor_y*) (list x y))
						((>= x *cursor_x*) (list x y)))) xs)) (list found)))
				(bind '(x y) next)
				(setq *anchor_x* x *anchor_y* y)
				(. *current_buffer* :set_cursor (+ x (length pattern)) y)
				(refresh)))))

(defun action-find-up ()
	(unless (eql (defq pattern (get :clear_text *find_text*)) "")
		(if (> (length (defq found (filter-whole-words (. *current_buffer* :find pattern) pattern))) 0)
			(when (defq next (some! (inc *cursor_y*) 0 nil (lambda (xs)
					(defq y _)
					(some-rev (lambda (x) (cond
						((< y *cursor_y*) (list x y))
						((< x *cursor_x*) (list x y)))) xs)) (list found)))
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
				(each-rev (lambda (xs)
					(defq y _)
					(each-rev (lambda (x)
						(.-> buffer (:set_cursor x y) (:cut (+ x (length pattern)) y))
						(unless (eql replace "") (. buffer :insert replace))) xs)) found))
			(clear-selection) (refresh))))
