;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defun action-home ()
	(input-cursor (. buffer :set_cursor *margin_x* y))
	(refresh))

(defun action-home-select ()
	(input-cursor (. buffer :set_cursor *margin_x* y ax ay))
	(refresh))

(defun action-end ()
	(input-cursor (. buffer :end))
	(refresh))

(defun action-end-select ()
	(input-cursor (. buffer :end_select))
	(refresh))

(defun action-left ()
	(input-cursor (if (> x *margin_x*)
		(. buffer :left) (. buffer :set_cursor x y)))
	(refresh))

(defun action-left-select ()
	(input-cursor (if (> x *margin_x*) (. buffer :left_select)))
	(refresh))

(defun action-right ()
	(input-cursor (if (/= x (dec (length (. buffer :get_text_line y))))
		(. buffer :right) (. buffer :set_cursor x y)))
	(refresh))

(defun action-right-select ()
	(input-cursor (if (/= x (dec (length (. buffer :get_text_line y))))
		(. buffer :right_select)))
	(refresh))

(defun action-up ()
	(unless *pipe*
		(input-cursor
			(bind '(history) (gather *meta_map* :history))
			(defq cmd "")
			(setq *history_idx* (max 0 (dec *history_idx*)))
			(when (< *history_idx* (length history))
				(setq cmd (elem-get history *history_idx*)))
			(.-> buffer
				(:set_cursor 0 y (dec (length (. buffer :get_text_line y))))
				(:insert (cat *env_terminal_prompt* cmd))))
		(refresh)))

(defun action-down ()
	(unless *pipe*
		(input-cursor
			(bind '(history) (gather *meta_map* :history))
			(defq cmd "")
			(setq *history_idx* (min (inc *history_idx*) (length history)))
			(when (< *history_idx* (length history))
				(setq cmd (elem-get history *history_idx*)))
			(.-> buffer
				(:set_cursor 0 y (dec (length (. buffer :get_text_line y))))
				(:insert (cat *env_terminal_prompt* cmd))))
		(refresh)))
