;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string)
	(. *current_buffer* :cut *anchor_x* *anchor_y*)
	(. *current_buffer* :insert string)
	(clear-selection) (refresh))

(defun action-backspace ()
	(when (eql (. *current_buffer* :cut *anchor_x* *anchor_y*) "")
		(. *current_buffer* :backspace))
	(clear-selection) (refresh))

(defun action-delete ()
	(when (eql (. *current_buffer* :cut *anchor_x* *anchor_y*) "")
		(. *current_buffer* :delete))
	(clear-selection) (refresh))

(defun action-break ()
	(undoable
		(. buffer :cut *anchor_x* *anchor_y*)
		(bind '(x y) (.-> buffer :break :get_cursor))
		(defq line (. buffer :get_text_line (dec y))
			indent (some (# (unless (eql %0 (ascii-char +char_space)) _)) line))
		(unless indent (setq indent (length line)))
		(if (> indent 0) (. buffer :insert (pad "" indent "        "))))
	(clear-selection) (refresh))

(defun action-tab ()
	(cond
		((= *anchor_y* *cursor_y*)
			(undoable
				(defq tab_width (. buffer :get_tab_width))
				(. buffer :cut *anchor_x* *anchor_y*)
				(bind '(x y) (. buffer :get_cursor))
				(. buffer :insert (pad "" (- tab_width (% x tab_width)) "        ")))
			(clear-selection) (refresh))
		((action-right-tab))))
