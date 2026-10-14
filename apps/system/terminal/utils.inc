;;;;;;;
; utils
;;;;;;;

(defmacro input-cursor (&rest _)
	`(progn
		(defq x *cursor_x* y *cursor_y* buffer (. *edit* :get_buffer))
		(bind '(& & ax ay) (. *edit* :get_cursor))
		(unless (= ay y) (setq ax x ay y))
		(. buffer :set_cursor x y ax ay)
		~_
		(bind '(x y &ignore) (. buffer :get_cursor))
		(setq *cursor_x* x *cursor_y* y)))

(defmacro defaction (method &rest args)
	(defq func (sym (cat "action-" (join (split (rest method) "_") "-"))))
	`(defun ,func (~args)
		(. *edit* ,method ~args)
		(refresh)))

(defun typed-line (y)
	; (typed-line y) -> str
	;what is typed on a line, after the prompt and before its end. A line
	;shorter than that, an empty one where a command's output left the
	;cursor, has nothing typed on it: the cut of it threw, and the Terminal
	;went with it
	(defq line (. buffer :get_text_line y))
	(if (> (length line) (inc *margin_x*)) (slice line *margin_x* -2) ""))
