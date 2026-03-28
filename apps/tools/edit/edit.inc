;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (text)
	(. *edit* :insert text)
	(refresh))

(defun action-backspace ()
	(. *edit* :backspace)
	(refresh))

(defun action-delete ()
	(. *edit* :delete)
	(refresh))

(defun action-break ()
	;line break and auto indent
	(defq buffer (. *edit* :get_buffer))
	(undoable buffer
		(reach (lambda (csr) (task-slice)
				(bind '(cx cy ax ay &ignore) (csr-sort csr))
				(bind '(cx cy ax ay &ignore) (. buffer :clip_cursor cx cy ax ay))
				(when (or (/= cx ax) (/= cy ay))
					(. buffer :idelete cx cy ax ay))
				(defq line (. buffer :get_text_line cy)
					indent_len (bskip +char_class_space line 0)
					indent_str (slice line 0 (min indent_len cx))
					trim_left (rbskip +char_class_space line cx)
					trim_right (bskip +char_class_space line cx))
				(. buffer :idelete trim_left cy trim_right cy)
				(. buffer :iinsert trim_left cy (cat +char_class_lf indent_str)))
			(. buffer :merge_cursors (. buffer :get_cursors))))
	(refresh))

(defun action-tab ()
	(defq buffer (. *edit* :get_buffer))
	; check if all cursors are point cursors
	(if (every (lambda ((cx cy ax ay &ignore)) (and (= cx ax) (= cy ay)))
			(. buffer :get_cursors))
		(progn
			(. *edit* :tab)
			(refresh))
		(action-right-tab)))
