;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string)
	(. *edit* :insert string))

(defun action-backspace ()
	(. *edit* :backspace))

(defun action-delete ()
	(. *edit* :delete))

(defun action-break ()
	(. *edit* :break))

(defun action-tab ()
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(if (= ay cy)
		(. *edit* :tab)
		(. *edit* :right_tab)))
