;;;;;;;;;;;;;;;;;;
; title widget
;;;;;;;;;;;;;;;;;;

(ffi create-title "gui/title/lisp_create" 0)

(class Title ()
	; (Title) -> title
	; overide the default 'this' env with a title component
	(defq this (create-title) label_flow (Flow) label_text (create-text))
	(def this :label_flow label_flow :label_text label_text)
	(view-set-flags label_flow 0 -1)
	(view-add-child this label_flow)
	(view-add-child label_flow label_text)

	(method :pref_size (this)
		; (. title :pref_size) -> (width height)
		(defq flow (get :label_flow this) border (abs (get :border this)))
		(bind '(w h) (view-pref-size flow))
		(list (+ (* border 2) w) (+ (* border 2) h)))

	(method :layout (this)
		; (. title :layout) -> title
		(defq flow (get :label_flow this) border (abs (get :border this))
			color (get :color this))
		(bind '(w h) (view-get-size this))
		(view-change flow border border (- w (* border 2)) (- h (* border 2)))
		(view-layout flow)
		(if (= (>> color 24) 0xff)
			(view-set-flags this view_flag_opaque view_flag_opaque))
		this)
	)
