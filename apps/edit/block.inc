;;;;;;;;;;;;;;;
; block actions
;;;;;;;;;;;;;;;

(defun action-left-tab ()
	(. *edit* :left_tab)
	(refresh))

(defun action-right-tab ()
	(. *edit* :right_tab)
	(refresh))

(defun action-to-lower ()
	(. *edit* :to_lower)
	(refresh))

(defun action-to-upper ()
	(. *edit* :to_upper)
	(refresh))

(defun action-reflow ()
	(. *edit* :reflow)
	(refresh))

(defun action-split ()
	(. *edit* :split)
	(refresh))

(defun action-sort ()
	(. *edit* :sort)
	(refresh))

(defun action-unique ()
	(. *edit* :unique)
	(refresh))

(defun action-invert ()
	(. *edit* :invert)
	(refresh))

(defun action-comment ()
	(. *edit* :comment)
	(refresh))
