;;;;;;;;;;;;;;;;;;
; textfield widget
;;;;;;;;;;;;;;;;;;

(import "gui/label/lisp.inc")
(import "gui/font/lisp.inc")

(defclass Textfield () (Label)
	; (Textfield) -> textfield
	(def this :cursor 0 :clear_text "" :hint_text "" :text "" :mode nil)

	(defmethod :draw (this)
		; (. textfield :draw) -> textfield
		(.super this :draw)
		(bind '(w h) (. this :get_size))
		(when (and (defq font (get :font this)) (defq text (get :text this)))
			(bind '(tw th) (font-glyph-bounds font (slice 0 (min (get :cursor this) (length text)) text)))
			(.-> this
				(:ctx_set_color (get :ink_color this))
				(:ctx_filled_box tw (>>> (- h th) 1) 2 th)))
		this)

	(defmethod :layout (this)
		; (. textfield :layout) -> textfield
		(cond
			((eql (defq text (get :clear_text this)) "")
				(defq text (get :hint_text this) mode nil)
				(def this :ink_color (get :hint_color this)))
			(t	(defq mode (get :mode this))
				(def this :ink_color (get :no_hint_color this))))
		(if mode
			(def this :text (pad "" (length text) "******************"))
			(def this :text text))
		(.super this :layout))

	(defmethod :key_down (this event)
		; (. textfield :key_down event) -> textfield
		(defq key (getf event +ev_msg_key_key) text (get :clear_text this)
			cursor (min (get :cursor this) (length text)))
		(cond
			((or (= key 10) (= key 13))
				;enter action
				(. this :emit))
			((= key 8)
				;backspace
				(and (/= cursor 0) (/= (length text) 0)
					(setq cursor (dec cursor) text (erase text cursor (inc cursor)))))
			((= key 127)
				;delete
				(and (/= cursor (length text)) (/= (length text) 0)
					(setq text (erase text cursor (inc cursor)))))
			((= key 0x40000050)
				;left
				(and (/= cursor 0)
					(setq cursor (dec cursor))))
			((= key 0x4000004f)
				;right
				(and (/= cursor (length text))
					(setq cursor (inc cursor))))
			((<= 32 key 126)
				;append key
				(setq text (insert text cursor (char key)) cursor (inc cursor))))
		(def this :clear_text text :cursor cursor)
		(.-> this :layout :dirty_all))
	)
