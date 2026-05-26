;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defaction :top)
(defaction :bottom)
(defaction :home)
(defaction :end)
(defaction :left)
(defaction :right)
(defaction :up)
(defaction :down)
(defaction :top_select)
(defaction :bottom_select)
(defaction :home_select)
(defaction :end_select)
(defaction :left_select)
(defaction :right_select)
(defaction :up_select)
(defaction :down_select)
(defaction :left_bracket)
(defaction :right_bracket)
(defaction :left_white_space)
(defaction :right_white_space)

(defun action-push ()
	(bind '(cx cy &ignore) (. *edit* :get_cursor))
	(defq pos (list *current_file* cx cy))
	(when (or (empty? *cursor_stack*)
			(nql (str (last *cursor_stack*)) (str pos)))
		(push *cursor_stack* pos)))

(defun action-pop ()
	(when (defq pos (pop *cursor_stack*))
		(bind '(file cx cy) pos)
		(when (or (eql :nil file) (/= 0 (age file)))
			;file exists or scratch
			(open-file file)
			(. *edit* :set_cursor cx cy)
			(refresh))))

(defun action-breakpoint (brk_id file line)
	;search backwards from line to find breakpoint
	(when (/= 0 (age file))
		;file exists !
		(open-file file)
		(when (defq pattern (cat "debug-brk \q" brk_id "\q")
				lines (.-> *edit* :get_buffer :get_buffer_lines)
				cy (min line (length lines))
				cy (some! (# (if (found? %0 pattern) (!))) (list lines)
						:nil cy (max 0 (- cy 256))))
			;found the breakpoint
			(.-> *edit* (:set_cursor 0 cy) :select_line)
			(refresh))))
