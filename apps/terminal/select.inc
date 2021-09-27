;;;;;;;;;;;;;;;;
; select actions
;;;;;;;;;;;;;;;;

(defun action-select-word ()
	(. *edit* :select_word)
	(refresh))

(defun action-select-line ()
	(. *edit* :select_line)
	(refresh))

(defun action-select-paragraph ()
	(. *edit* :select_paragraph)
	(refresh))
