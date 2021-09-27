;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string)
	(. *edit* :insert string)
	(clear-selection) (refresh))

(defun action-backspace ()
	(. *edit* :backspace)
	(clear-selection) (refresh))

(defun action-delete ()
	(. *edit* :delete)
	(clear-selection) (refresh))

(defun action-break ()
	(. *edit* :break)
	(clear-selection) (refresh))

(defun action-tab ()
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(cond
		((= ay cy)
			(. *edit* :tab)
			(clear-selection))
		((. *edit* :right_tab)))
	(refresh))
