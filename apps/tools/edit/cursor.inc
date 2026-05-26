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
(defaction :top-select)
(defaction :bottom-select)
(defaction :home-select)
(defaction :end-select)
(defaction :left-select)
(defaction :right-select)
(defaction :up-select)
(defaction :down-select)
(defaction :left-bracket)
(defaction :right-bracket)
(defaction :left-white-space)
(defaction :right-white-space)

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
