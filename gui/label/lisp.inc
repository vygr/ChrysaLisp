;;;;;;;;;;;;;;;
; label widget
;;;;;;;;;;;;;;;

(ffi create-label "gui/label/lisp_create" 0)

(class Label ()
	; (Label) -> label
	; overide the default 'this' env with a label component
	(defq this (create-label) flow (Flow) text (create-text))
	(set-field this (const label_flow) 0 flow)
	(set-field this (const label_text) 0 text)
	(view-set-flags flow 0 -1)
	(view-add-child this flow)
	(view-add-child flow text)

	(method :pref_size (this)
		; (. label :pref_size) -> (width height)
		(defq flow (get-field this (const label_flow) 0) border (abs (get :border this))
			mw (get :min_width this) mh (get :min_height this) w 0 h 0)
		(unless mw (setq mw 0))
		(unless mh (setq mh 0))
		(if flow (bind '(w h) (view-pref-size flow)))
		(list (+ (* border 2) (max w mw)) (+ (* border 2) (max h mh))))

	(method :layout (this)
		; (. label :layout) -> label
		(defq flow (get-field this (const label_flow) 0)
			border (abs (get :border this)) color (get :color this))
		(bind '(w h) (view-get-size this))
		(view-change flow border border (- w (* border 2)) (- h (* border 2)))
		(view-layout flow)
		(if (= (>> color 24) 0xff)
			(view-set-flags this view_flag_opaque view_flag_opaque))
		this)
	)
