;;;;;;;;;;;;;;;
; label widget
;;;;;;;;;;;;;;;

(ffi create-label "gui/label/lisp_create" 0)

(class Label ()
	; (Label) -> label
	; overide the default 'this' env with a label component
	(defq this (create-label) label_flow (Flow) label_text (create-text))
	(def this :label_flow label_flow :label_text label_text :min_width 0 :min_height 0)
	(view-set-flags label_flow 0 -1)
	(view-add-child this label_flow)
	(view-add-child label_flow label_text)

	(method :pref_size (this)
		; (. label :pref_size) -> (width height)
		(defq flow (get :label_flow this) border (abs (get :border this))
			mw (get :min_width this) mh (get :min_height this))
		(bind '(w h) (. flow :pref_size))
		(list (+ (* border 2) (max w mw)) (+ (* border 2) (max h mh))))

	(method :layout (this)
		; (. label :layout) -> label
		(defq flow (get :label_flow this) border (abs (get :border this))
			color (get :color this))
		(bind '(w h) (view-get-size this))
		(view-change flow border border (- w (* border 2)) (- h (* border 2)))
		(view-layout flow)
		(if (= (>> color 24) 0xff)
			(view-set-flags this +view_flag_opaque+ +view_flag_opaque+))
		this)
	)
