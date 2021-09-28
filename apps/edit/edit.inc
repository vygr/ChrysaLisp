;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string)
	(. *edit* :insert string)
	(refresh))

(defun action-backspace ()
	(. *edit* :backspace)
	(refresh))

(defun action-delete ()
	(. *edit* :delete)
	(refresh))

(defun action-break ()
	(. *edit* :break)
	(refresh))

(defun action-tab ()
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(cond
		((= ay cy) (. *edit* :tab))
		((. *edit* :right_tab)))
	(refresh))
