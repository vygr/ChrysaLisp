;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defaction :insert text)
(defaction :backspace)
(defaction :delete)
(defaction :break)

(defun action-tab ()
	(defq buffer (. *edit* :get_buffer))
	; check if all cursors are point cursors
	(if (every (lambda ((cx cy ax ay &ignore)) (and (= cx ax) (= cy ay)))
			(. buffer :get_cursors))
		(progn
			(. *edit* :tab)
			(refresh))
		(action-right-tab)))
