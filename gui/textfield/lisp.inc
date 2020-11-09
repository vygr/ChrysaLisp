;;;;;;;;;;;;;;;;;;
; textfield widget
;;;;;;;;;;;;;;;;;;

(ffi create-textfield "gui/textfield/lisp_create" 0)

(class Textfield ()
	; (Textfield) -> textfield
	; overide the default 'this' env with a textfield component
	(defq this (create-textfield) label_flow (Flow) label_text (create-text))
	(def this :label_flow label_flow :label_text label_text :cursor 0)
	(view-set-flags label_flow 0 -1)
	(view-add-child this label_flow)
	(view-add-child label_flow label_text)

	(method :pref_size (this)
		; (. textfield :pref_size) -> (width height)
		(defq border (abs (get :border this)))
		(bind '(w h) (. (get :label_flow this) :pref_size))
		(list (+ (* border 2) w) (+ (* border 2) h)))

	(method :layout (this)
		; (. textfield :layout) -> textfield
		(defq flow (get :label_flow this) border (abs (get :border this))
			color (get :color this))
		(bind '(w h) (view-get-size this))
		(view-change flow border border (- w (* border 2)) (- h (* border 2)))
		(view-layout flow)
		(if (= (>> color 24) 0xff)
			(view-set-flags this view_flag_opaque view_flag_opaque))
		this)

	(method :key_down (this event)
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
		(view-dirty-all (view-layout this)))
	)
