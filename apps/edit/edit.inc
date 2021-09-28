;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string)
	(.-> *edit* (:insert string) :clear_selection)
	(refresh))

(defun action-backspace ()
	(.-> *edit* :backspace :clear_selection)
	(refresh))

(defun action-delete ()
	(.-> *edit* :delete :clear_selection)
	(refresh))

(defun action-break ()
	(.-> *edit* :break :clear_selection)
	(refresh))

(defun action-tab ()
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(cond
		((= ay cy)
			(.-> *edit* :tab :clear_selection))
		((. *edit* :right_tab)))
	(refresh))
