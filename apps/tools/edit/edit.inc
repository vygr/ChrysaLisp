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
	;FIXME to do block mode
	(. *edit* :tab)
	(refresh))
