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
		(. syntax :set_state :text)
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
			(each (lambda (line)
					(push buffer_colored (. syntax :colorise line)))
				(get :buffer this))
			(set this :buffer_colored buffer_colored :dirty nil))
		(. vdu :load
			(get :buffer_colored this)
			(get :scroll_x this) (get :scroll_y this)
			(get :cursor_x this) (get :cursor_y this))
		this)
	)

;module
(export (penv)
	*class_Buffer* Buffer Buffer?)
(env-pop)
