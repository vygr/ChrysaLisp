;;;;;;;;;;;;;;;;
; select actions
;;;;;;;;;;;;;;;;

(defun action-select-word ()
	(. *edit* :select_word))

(defun action-select-line ()
	(. *edit* :select_line))

(defun action-select-paragraph ()
	(. *edit* :select_paragraph))
