;;;;;;;;;;;;;;;
; block actions
;;;;;;;;;;;;;;;

(defq +block_comment ";; ")

(defun action-left-tab ()
	(undoable
		(defq tab_width (. buffer :get_tab_width))
		(bind '(y y1) (select-lines))
		(each! y y1 (lambda (line)
				(task-slice)
				(defq idx -1)
				(while (and
					(< (setq idx (inc idx)) (length line))
					(< idx tab_width)
					(eql (elem idx line) " ")))
				(if (> idx 0)
					(.-> buffer (:set_cursor 0 _) (:delete (min tab_width idx)))))
			(list (. buffer :get_text_lines)))
		(. buffer :set_cursor *cursor_x* *cursor_y*))
	(refresh))

(defun action-right-tab ()
	(undoable
		(defq tab (pad "" (. buffer :get_tab_width) "        "))
		(bind '(y y1) (select-lines))
		(each! y y1 (lambda (line)
				(task-slice)
				(unless (eql line "")
					(.-> buffer (:set_cursor 0 _) (:insert tab))))
			(list (. buffer :get_text_lines)))
		(. buffer :set_cursor *cursor_x* *cursor_y*))
	(refresh))

(defun action-reflow ()
	(undoable
		(bind '(y y1) (select-paragraph))
		(each (lambda (line)
				(task-slice)
				(.-> buffer (:insert line) :break))
			(. (. buffer :get_syntax) :text_flow
				(split (.-> buffer (:set_cursor 0 y) (:cut 0 y1))
					(const (cat " " (ascii-char +char_lf))))
				(. buffer :get_wrap_width)))
		(bind '(x y) (. buffer :get_cursor))
		(bind '(x y) (. buffer :constrain x (inc y)))
		(. buffer :set_cursor x y))
	(clear-selection) (refresh))

(defun action-to-lower ()
	(when (selection?)
		(undoable
			(. buffer :paste (to-lower (. buffer :cut *anchor_x* *anchor_y*))))
		(refresh)))

(defun action-to-upper ()
	(when (selection?)
		(undoable
			(. buffer :paste (to-upper (. buffer :cut *anchor_x* *anchor_y*))))
		(refresh)))

(defun action-ordered ()
	(when (selection?)
		(undoable
			(bind '(y y1) (select-lines))
			(setq *anchor_x* 0 *anchor_y* y)
			(. buffer :paste (cat (join
				(sort cmp (split (.-> buffer (:set_cursor 0 y1) (:cut 0 y)) (ascii-char +char_lf)))
				(ascii-char +char_lf)) (ascii-char +char_lf))))
		(refresh)))

(defun action-ordered-unique ()
	(when (selection?)
		(undoable
			(bind '(y y1) (select-lines))
			(setq *anchor_x* 0 *anchor_y* y)
			(. buffer :paste (cat (join
				(reduce (# (if (or (<= (length %0) 0)
								(not (eql (elem -2 %0) %1))) (push %0 %1) %0))
				(sort cmp (split (.-> buffer (:set_cursor 0 y1) (:cut 0 y)) (ascii-char +char_lf)))
				(list)) (ascii-char +char_lf)) (ascii-char +char_lf))))
		(refresh)))

(defun action-comment-block ()
	(undoable
		(bind '(y y1) (select-lines))
		(setq *anchor_x* 0 *anchor_y* y)
		(each! y y1 (lambda (line)
				(task-slice)
				(cond
					((blank-line? line))
					((starts-with +block_comment line)
						(.-> buffer (:set_cursor 0 _)
							(:delete (const (length +block_comment)))))
					((.-> buffer (:set_cursor 0 _)
							(:insert +block_comment)))))
			(list (. buffer :get_text_lines)))
		(bind '(x y) (. buffer :get_cursor))
		(bind '(x y) (. buffer :constrain 0 (inc y)))
		(. buffer :set_cursor x y))
	(refresh))
