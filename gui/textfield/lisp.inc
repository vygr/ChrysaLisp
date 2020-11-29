;;;;;;;;;;;;;;;;;;
; textfield widget
;;;;;;;;;;;;;;;;;;

(import "gui/label/lisp.inc")

(defclass Textfield ()
	; (Textfield) -> textfield
	; overide the default 'this' env with a textfield component
	; inherit from Label !
	(def (defq this (Label)) :cursor 0)

	;draw function for compositor...
	(deffimethod :draw "gui/textfield/draw")

	(defmethod :key_down (this event)
		; (. textfield :key_down event) -> textfield
		(defq key (get-uint event ev_msg_key_key) cursor (get :cursor this)
			text (get :text this))
		(cond
			((or (= key 10) (= key 13))
				;enter action
				(component-emit this))
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
		(def this :text text :cursor cursor)
		(.-> this :layout :dirty_all))
	)
