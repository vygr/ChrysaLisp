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

(defun action-ordered ()
	(. *edit* :ordered)
	(refresh))

(defun action-ordered-unique ()
	(. *edit* :ordered_unique)
	(refresh))

(defun action-comment-block ()
	(. *edit* :comment)
	(refresh))
