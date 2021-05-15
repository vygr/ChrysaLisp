;;;;;;;;;;;;;;;;;;;;
; text buffer object
;;;;;;;;;;;;;;;;;;;;

(import "lib/text/syntax.inc")

;module
(env-push)

(defun check_spaning_states (string)
	(if (or (find (ascii-char 34) string)
			(find "{" string)
			(find "}" string))
		(setq buffer_colored (cat buffer))))

(defmacro mutate_buffer (&rest _)
	`(progn
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(defq buffer (get :buffer this)
			buffer_colored (get :buffer_colored this)
			buffer_states (get :buffer_states this)
			h (length buffer))
		~_
		(set this :buffer buffer :buffer_colored buffer_colored
			:buffer_states buffer_states :dirty t)
		(. this :set_cursor x y)))

(defclass Buffer () nil
	; (Buffer) -> buffer
	(def this :buffer (list) :buffer_colored (list) :buffer_states (list)
		:dirty nil :syntax (Syntax)
		:cursor_x 0 :cursor_y 0 :scroll_x 0 :scroll_y 0
		:max_width 0 :tab_width 4)

	(defmethod :set_scroll (this x y)
		; (. buffer :set_scroll x y) -> buffer
		(set this :scroll_x x :scroll_y y)
		this)

	(defmethod :get_scroll (this)
		; (. buffer :get_scroll) -> (x y)
		(list (get :scroll_x this) (get :scroll_y this)))

	(defmethod :set_cursor (this x y)
		; (. buffer :set_cursor x y) -> buffer
		(set this :cursor_x x :cursor_y y)
		this)

	(defmethod :get_cursor (this)
		; (. buffer :get_cursor) -> (x y)
		(list (get :cursor_x this) (get :cursor_y this)))

	(defmethod :get_size (this)
		; (. buffer :get_size) -> (width height)
		(list (get :max_width this) (length (get :buffer this))))

	(defmethod :file_load (this filepath)
		; (. buffer :file_load filepath) -> buffer
		(defq syntax (get :syntax this)
			buffer (list) buffer_colored (list) buffer_states (list)
			max_width 0 tab_width (get :tab_width this))
		(each-line (lambda (line)
				(setq line (. syntax :expand_tabs line tab_width)
					max_width (max max_width (length line)))
				(push buffer line)
				(push buffer_colored line)
				(push buffer_states '(nil nil)))
			(file-stream filepath))
		(set this :buffer buffer :buffer_colored buffer_colored
			:buffer_states buffer_states :max_width max_width :dirty t)
		this)

	(defmethod :vdu_load (this vdu)
		; (. buffer :vdu_load vdu) -> buffer
		(when (get :dirty this)
			(defq syntax (get :syntax this)
				buffer (get :buffer this)
				buffer_colored (get :buffer_colored this)
				buffer_states (get :buffer_states this)
				state :text)
			(each (lambda (line line_array (start_state end_state))
					(cond
						((and (array? line_array) (eql state start_state))
							(setq state end_state))
						(t	(. syntax :set_state state)
							(elem-set _ buffer_colored (. syntax :colorise line))
							(elem-set _ buffer_states (list state
								(setq state (. syntax :get_state)))))))
				buffer buffer_colored buffer_states)
			(set this :dirty nil))
		(. vdu :load
			(get :buffer_colored this)
			(get :scroll_x this) (get :scroll_y this)
			(get :cursor_x this) (get :cursor_y this))
		this)

	(defmethod :constrain (this x y)
		; (. buffer :constrain x y) -> (list x y)
		(defq buffer (get :buffer this) h (length buffer))
		(setq y (min (max y 0) h)
			x (max 0 (if (>= y h) 0 (min x (length (elem y buffer))))))
		(list x y))

	(defmethod :cursor_left (this)
		; (. buffer :cursor_left) -> buffer
		(bind '(x y) (. this :get_cursor))
		(if (< (setq x (dec x)) 0)
			(if (< (setq y (dec y)) 0)
				(setq x 0)
				(setq x +max_int)))
		(bind '(x y) (. this :constrain x y))
		(. this :set_cursor x y))

	(defmethod :cursor_right (this)
		; (. buffer :cursor_right) -> buffer
		(bind '(x y) (. this :get_cursor))
		(defq buffer (get :buffer this) h (length buffer)
			w (if (>= y h) 0 (length (elem y buffer))))
		(if (> (setq x (inc x)) w)
			(if (> (setq y (inc y)) h)
				(setq x +max_int)
				(setq x 0)))
		(bind '(x y) (. this :constrain x y))
		(. this :set_cursor x y))

	(defmethod :cursor_down (this)
		; (. buffer :cursor_down) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x (inc y)))
		(. this :set_cursor x y))

	(defmethod :cursor_up (this)
		; (. buffer :cursor_up) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x (dec y)))
		(. this :set_cursor x y))

	(defmethod :break (this)
		; (. buffer :break) -> buffer
		(mutate_buffer
			(cond
				((>= y h)
					(setq buffer (insert buffer y '("")))
					(setq buffer_colored (insert buffer_colored y '("")))
					(setq buffer_states (insert buffer_states y '((nil nil)))))
				(t	(defq front (slice 0 x (elem y buffer))
						back (slice x -1 (elem y buffer)))
					(elem-set y buffer front)
					(elem-set y buffer_colored front)
					(elem-set y buffer_states '(nil nil))
					(setq buffer (insert buffer (inc y) (list back)))
					(setq buffer_colored (insert buffer_colored (inc y) (list back)))
					(setq buffer_states (insert buffer_states (inc y) '((nil nil))))))
			(setq x 0 y (inc y))))

	(defmethod :backspace (this)
		; (. buffer :backspace) -> buffer
		(mutate_buffer
			(cond
				((and (<= x 0) (<= y 0)))
				((>= y h)
					(setq y (dec y) x (length (elem y buffer))))
				((<= x 0)
					(setq y (dec y) x (length (elem y buffer)))
					(elem-set y buffer (cat (elem y buffer) (elem (inc y) buffer)))
					(elem-set y buffer_colored (elem y buffer))
					(setq buffer (erase buffer (inc y) (+ y 2)))
					(setq buffer_colored (erase buffer_colored (inc y) (+ y 2)))
					(setq buffer_states (erase buffer_states (inc y) (+ y 2))))
				(t	(check_spaning_states (slice (dec x) x (elem y buffer)))
					(elem-set y buffer (erase (elem y buffer) (dec x) x))
					(elem-set y buffer_colored (elem y buffer))
					(setq x (dec x))))))

	(defmethod :delete (this)
		; (. buffer :delete) -> buffer
		(mutate_buffer
			(cond
				((>= y h))
				((>= x (length (elem y buffer)))
					(when (< y (dec h))
						(elem-set y buffer (cat (elem y buffer) (elem (inc y) buffer)))
						(elem-set y buffer_colored (elem y buffer))
						(setq buffer (erase buffer (inc y) (+ y 2)))
						(setq buffer_colored (erase buffer_colored (inc y) (+ y 2)))
						(setq buffer_states (erase buffer_states (inc y) (+ y 2)))))
				(t	(check_spaning_states (slice x (inc x) (elem y buffer)))
					(elem-set y buffer (erase (elem y buffer) x (inc x)))
					(elem-set y buffer_colored (elem y buffer))))))

	(defmethod :insert (this string)
		; (. buffer :insert string) -> buffer
		(mutate_buffer
			(cond
				((>= y h)
					(setq buffer (insert buffer y (list string)))
					(setq buffer_colored (insert buffer_colored y (list string)))
					(setq buffer_states (insert buffer_states y '((nil nil)))))
				(t	(elem-set y buffer (insert (elem y buffer) x string))
					(elem-set y buffer_colored (elem y buffer))))
			(check_spaning_states string)
			(setq x (+ x (length string)))))
	)

;module
(export (penv)
	*class_Buffer* Buffer Buffer?)
(env-pop)
