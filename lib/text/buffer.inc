;;;;;;;;;;;;;;;;;;;;
; text buffer object
;;;;;;;;;;;;;;;;;;;;

(import "lib/text/syntax.inc")

;module
(env-push)

(defclass Buffer () nil
	; (Buffer) -> buffer
	(def this :buffer (list) :buffer_colored (list) :dirty nil :syntax (Syntax)
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
		(defq syntax (get :syntax this) buffer (list)
			max_width 0 tab_width (get :tab_width this))
		(each-line (lambda (line)
				(setq line (. syntax :expand_tabs line tab_width)
					max_width (max max_width (length line)))
				(push buffer line))
			(file-stream filepath))
		(set this :dirty t :buffer buffer :max_width max_width)
		this)

	(defmethod :vdu_load (this vdu)
		; (. buffer :vdu_load vdu) -> buffer
		(when (get :dirty this)
			(defq buffer_colored (list) syntax (get :syntax this))
			(. syntax :set_state :text)
			(each (lambda (line)
					(push buffer_colored (. syntax :colorise line)))
				(get :buffer this))
			(set this :buffer_colored buffer_colored :dirty nil))
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
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(defq buffer (get :buffer this) h (length buffer))
		(cond
			((>= y h)
				(set this :buffer (insert buffer y '(""))))
			(t	(defq front (slice 0 x (elem y buffer))
					back (slice x -1 (elem y buffer)))
				(elem-set y buffer front)
				(set this :buffer (insert buffer (inc y) (list back)))))
		(set this :dirty t)
		(. this :set_cursor 0 (inc y)))

	(defmethod :backspace (this)
		; (. buffer :backspace) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(defq buffer (get :buffer this) h (length buffer))
		(cond
			((and (<= x 0) (<= y 0)))
			((>= y h)
				(setq y (dec y) x (length (elem y buffer))))
			((<= x 0)
				(setq y (dec y) x (length (elem y buffer)))
				(elem-set y buffer (cat (elem y buffer) (elem (inc y) buffer)))
				(set this :buffer (erase buffer (inc y) (+ y 2))))
			(t	(elem-set y buffer (erase (elem y buffer) (dec x) x))
				(setq x (dec x))))
		(set this :dirty t)
		(. this :set_cursor x y))

	(defmethod :delete (this)
		; (. buffer :delete) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(defq buffer (get :buffer this) h (length buffer))
		(cond
			((>= y h))
			((>= x (length (elem y buffer)))
				(when (< y (dec h))
					(elem-set y buffer (cat (elem y buffer) (elem (inc y) buffer)))
					(set this :buffer (erase buffer (inc y) (+ y 2)))))
			(t	(elem-set y buffer (erase (elem y buffer) x (inc x)))))
		(set this :dirty t)
		(. this :set_cursor x y))

	(defmethod :insert (this string)
		; (. buffer :insert string) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(defq buffer (get :buffer this) h (length buffer))
		(cond
			((>= y h)
				(set this :buffer (insert buffer y (list string))))
			(t	(elem-set y buffer (insert (elem y buffer) x string))))
		(set this :dirty t)
		(. this :set_cursor (+ x (length string)) y))
	)

;module
(export (penv)
	*class_Buffer* Buffer Buffer?)
(env-pop)
