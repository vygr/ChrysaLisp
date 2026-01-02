;;;;;;;;;;;;;;;;
; select actions
;;;;;;;;;;;;;;;;

(defun action-select-all ()
	(. *edit* :select_all)
	(refresh))

(defun action-select-word ()
	(. *edit* :select_word)
	(refresh))

(defun action-select-line ()
	(. *edit* :select_line)
	(refresh))

(defun action-select-paragraph ()
	(. *edit* :select_paragraph)
	(refresh))

(defun action-select-block ()
	(. *edit* :select_block)
	(refresh))
