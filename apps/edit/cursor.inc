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
		(when (or (eql :nil file) (/= 0 (age file)))
			;file exists or scratch
			(open-file file)
			(.-> *edit* (:set_cursor cx cy) :clear_selection)
			(refresh))))

(defun action-breakpoint (brk_id file line)
	;search backwards from line to find breakpoint
	(when (/= 0 (age file))
		;file exists !
		(open-file file)
		(when (defq pattern (cat {debug-brk "} brk_id {"})
				lines (.-> *edit* :get_buffer :get_text_lines)
				cy (min line (length lines))
				cy (some! (# (if (found? %0 pattern) (!))) (list lines)
						:nil cy (max 0 (- cy 256))))
			;found the breakpoint
			(.-> *edit* (:set_cursor 0 cy) :select_line)
			(refresh))))
