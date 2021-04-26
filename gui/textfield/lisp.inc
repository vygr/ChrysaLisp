;;;;;;;;;;;;;;;;;;
; textfield widget
;;;;;;;;;;;;;;;;;;

(import "gui/label/lisp.inc")
(import "gui/font/lisp.inc")

(defclass Textfield () (Label)
	; (Textfield) -> textfield
	(def this :cursor 0 :clear_text "" :mode nil)

	(defmethod :draw (this)
		; (. textfield :draw) -> textfield
		(.super this :draw)
		(bind '(w h) (. this :get_size))
		(when (and (defq font (get :font this)) (defq text (get :text this)))
			(bind '(tw th) (font-glyph-bounds font (slice 0 (min (get :cursor this) (length text)) text)))
			(.-> this (:ctx_set_color +argb_black) (:ctx_filled_box tw (>>> (- h th) 1) 2 th)))
		this)

	(defmethod :layout (this)
		; (. textfield :layout) -> textfield
		(if (get :mode this)
			(def this :text (pad "" (length (get :clear_text this)) "******************"))
			(def this :text (get :clear_text this)))
		(.super this :layout))

	(defmethod :key_down (this event)
		; (. textfield :key_down event) -> textfield
		(defq key (getf event +ev_msg_key_key) text (get :clear_text this)
			cursor (min (get :cursor this) (length text)))
		(cond
			((or (= key 10) (= key 13))
				;enter action
				(. this :emit))
			((and (= key 8) (/= cursor 0) (/= (length text) 0))
				;backspace
				(setq cursor (dec cursor) text (erase text cursor (inc cursor))))
			((and (= key 0x40000050) (/= cursor 0))
				;left
				(setq cursor (dec cursor)))
			((and (= key 0x4000004f) (/= cursor (length text)))
				;right
				(setq cursor (inc cursor)))
			((<= 32 key 127)
				;append key
				(setq text (insert text cursor (char key)) cursor (inc cursor))))
		(def this :clear_text text :cursor cursor)
		(.-> this :layout :dirty_all))
	)
