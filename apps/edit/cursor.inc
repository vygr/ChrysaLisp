;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defun action-home ()
	(. *edit* :home))

(defun action-end ()
	(. *edit* :end))

(defun action-left ()
	(. *edit* :left))

(defun action-right ()
	(. *edit* :right))

(defun action-up ()
	(. *edit* :up))

(defun action-down ()
	(. *edit* :down))

(defun action-home-select ()
	(. *edit* :home_select))

(defun action-end-select ()
	(. *edit* :end_select))

(defun action-left-select ()
	(. *edit* :left_select))

(defun action-right-select ()
	(. *edit* :right_select))

(defun action-up-select ()
	(. *edit* :up_select))

(defun action-down-select ()
	(. *edit* :down_select))

(defun action-left-bracket ()
	(. *edit* :left_bracket))

(defun action-right-bracket ()
	(. *edit* :right_bracket))
