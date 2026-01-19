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
	(. *edit* :insert +char_class_lf)
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
