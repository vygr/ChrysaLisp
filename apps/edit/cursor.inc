;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defun action-top ()
	(. *edit* :top)
	(refresh))

(defun action-bottom ()
	(. *edit* :bottom)
	(refresh))

(defun action-home ()
	(. *edit* :home)
	(refresh))

(defun action-end ()
	(. *edit* :end)
	(refresh))

(defun action-left ()
	(. *edit* :left)
	(refresh))

(defun action-right ()
	(. *edit* :right)
	(refresh))

(defun action-up ()
	(. *edit* :up)
	(refresh))

(defun action-down ()
	(. *edit* :down)
	(refresh))

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
	(refresh))

(defun action-right-bracket ()
	(. *edit* :right_bracket)
	(refresh))

(defun action-push ()
	(bind '(cx cy) (. *edit* :get_cursor))
	(defq pos (list *current_file* cx cy))
	(when (or (empty? *cursor_stack*)
			(not (eql (str (last *cursor_stack*)) (str pos))))
		(push *cursor_stack* pos)))

(defun action-pop ()
	(when (defq pos (pop *cursor_stack*))
		(bind '(file cx cy) pos)
		(when (/= 0 (age file))
			;file exists
			(open-file file)
			(.-> *edit* (:set_cursor cx cy) :clear_selection)
			(refresh))))
