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

(defmacro undoable (buffer &rest _)
	`(progn
		(unless buffer (setq buffer (. *edit* :get_buffer)))
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

(defun select-word ()
	(bind '(cx cy) (. *edit* :get_cursor))
	(defq line (.-> *edit* :get_buffer (:get_text_line cy))
		x (some! cx 0 nil (#
			(if (not-whole-word-char? %0) (inc _))) (list line))
		x1 (some! cx -1 nil (#
		   (if (not-whole-word-char? %0) _)) (list line)))
	(unless x (setq x 0))
	(unless x1 (setq x1 (length line)))
	(list x x1))
