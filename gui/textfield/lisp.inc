;;;;;;;;;;;;;;;;;;
; textfield widget
;;;;;;;;;;;;;;;;;;

(ffi create-textfield "gui/textfield/lisp_create" 0)

(class Textfield ()
	; (Textfield) -> textfield
	; overide the default 'this' env with a textfield component
	(defq this (create-textfield) label_flow (Flow) label_text (create-text))
	(def this :label_flow label_flow :label_text label_text)
	(view-set-flags label_flow 0 -1)
	(view-add-child this label_flow)
	(view-add-child label_flow label_text)

	(method :pref_size (this)
		; (. textfield :pref_size) -> (width height)
		(defq border (abs (get :border this)))
		(bind '(w h) (view-pref-size (get :label_flow this)))
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
	)
