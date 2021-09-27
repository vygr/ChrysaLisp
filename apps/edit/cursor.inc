;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defun action-home ()
	(. *edit* :home)
	(clear-selection) (refresh))

(defun action-end ()
	(. *edit* :end)
	(clear-selection) (refresh))

(defun action-left ()
	(. *edit* :left)
	(clear-selection) (refresh))

(defun action-right ()
	(. *edit* :right)
	(clear-selection) (refresh))

(defun action-up ()
	(. *edit* :up)
	(clear-selection) (refresh))

(defun action-down ()
	(. *edit* :down)
	(clear-selection) (refresh))

(defun action-home-select ()
	(. *edit* :home_select)
	(refresh))

(defun action-end-select ()
	(. *edit* :end_select)
	(refresh))

(defun action-left-select ()
	(. *edit* :left_select)
	(refresh))

(defun action-right-select ()
	(. *edit* :right_select)
	(refresh))

(defun action-up-select ()
	(. *edit* :up_select)
	(refresh))

(defun action-down-select ()
	(. *edit* :down_select)
	(refresh))

(defun action-left-bracket ()
	(. *edit* :left_bracket)
	(clear-selection) (refresh))

(defun action-right-bracket ()
	(. *edit* :right_bracket)
	(clear-selection) (refresh))
