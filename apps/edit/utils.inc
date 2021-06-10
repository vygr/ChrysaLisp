;;;;;;;
; utils
;;;;;;;

(defmacro not-whole-word-char? (_)
	`(find ,_ +not_whole_word_chars))

(defun blank-line? (line)
	(every (# (eql " " %0)) line))

(defun radio-select (toolbar idx)
	(each (lambda (button)
			(undef (. button :dirty) :color)
			(if (= _ idx) (def button :color *env_radio_col*)))
		(. toolbar :children)) idx)

(defmacro undoable (&rest _)
	`(progn
		(. (defq buffer *current_buffer*) :push_undo
			(list :mark (defq mark (. buffer :next_mark)))
			(list :cursor *cursor_x* *cursor_y*))
		~_
		(. buffer :push_undo (list :mark mark))))

(defun selection? ()
	(or (/= *anchor_x* *cursor_x*) (/= *anchor_y* *cursor_y*)))

(defun sort-selection ()
	(defq sx *cursor_x* sy *cursor_y* sx1 *anchor_x* sy1 *anchor_y*)
	(if (> sy sy1) (defq st sx sx sx1 sx1 st st sy sy sy1 sy1 st))
	(and (= sy sy1) (> sx sx1) (defq st sx sx sx1 sx1 st))
	(list sx sy sx1 sy1))

(defun select-lines ()
	(bind '(sx sy sx1 sy1) (sort-selection))
	(if (or (> sx1 0) (= sy sy1)) (setq sy1 (inc sy1)))
	(list sy sy1))

(defun select-word ()
	(defq line (. *current_buffer* :get_text_line *cursor_y*)
		x (some! *cursor_x* 0 nil (#
			(if (not-whole-word-char? %0) (inc _))) (list line))
		x1 (some! *cursor_x* -1 nil (#
		   (if (not-whole-word-char? %0) _)) (list line)))
	(unless x (setq x 0))
	(unless x1 (setq x1 (length line)))
	(list x x1))

(defun select-paragraph ()
	(defq y (some! *cursor_y* 0 nil
				(# (if (blank-line? %0) (inc _)))
			(list (. *current_buffer* :get_text_lines)))
		y1 (some! *cursor_y* (bind '(w h) (. *current_buffer* :get_size)) nil
				(# (if (blank-line? %0) _))
			(list (. *current_buffer* :get_text_lines))))
	(unless y (setq y 0))
	(unless y1 (setq y1 h))
	(list y y1))
