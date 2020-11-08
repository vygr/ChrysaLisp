;;;;;;;;;;;;;;;;;;
; textfield widget
;;;;;;;;;;;;;;;;;;

(ffi create-textfield "gui/textfield/lisp_create" 0)

(class Textfield ()
	; (Textfield) -> textfield
	; overide the default 'this' env with a textfield component
	(defq this (create-textfield) flow (Flow) text (create-text))
	(set-field this (const label_flow) 0 flow)
	(set-field this (const label_text) 0 text)
	(view-set-flags flow 0 -1)
	(view-add-child this flow)
	(view-add-child flow text)

	(method :pref_size (this)
		; (. textfield :pref_size) -> (width height)
		(defq flow (get-field this (const label_flow) 0) border (abs (get :border this))
			w 0 h 0)
		(if flow (bind '(w h) (view-pref-size flow)))
		(list (+ (* border 2) w) (+ (* border 2) h)))

	(method :layout (this)
		; (. textfield :layout) -> textfield
		(defq flow (get-field this (const label_flow) 0)
			border (abs (get :border this)) color (get :color this))
		(bind '(w h) (view-get-size this))
		(view-change flow border border (- w (* border 2)) (- h (* border 2)))
		(view-layout flow)
		(if (= (>> color 24) 0xff)
			(view-set-flags this view_flag_opaque view_flag_opaque))
		this)
	)
