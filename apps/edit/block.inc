;;;;;;;;;;;;;;;
; block actions
;;;;;;;;;;;;;;;

(defun action-left-tab ()
	(. *edit* :left_tab))

(defun action-right-tab ()
	(. *edit* :right_tab))

(defun action-to-lower ()
	(. *edit* :to_lower))

(defun action-to-upper ()
	(. *edit* :to_upper))

(defun action-reflow ()
	(. *edit* :reflow))

(defun action-ordered ()
	(. *edit* :ordered))

(defun action-ordered-unique ()
	(. *edit* :ordered_unique))

(defun action-comment-block ()
	(. *edit* :comment))
