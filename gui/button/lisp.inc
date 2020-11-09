;;;;;;;;;;;;;;;
; button widget
;;;;;;;;;;;;;;;

(ffi create-button "gui/button/lisp_create" 0)

(class Button ()
	; (Button) -> button
	; overide the default 'this' env with a button component
	(defq this (create-button) label_flow (Flow) label_text (create-text))
	(def this :label_flow label_flow :label_text label_text :state 0)
	(view-set-flags label_flow 0 -1)
	(view-add-child this label_flow)
	(view-add-child label_flow label_text)

	(method :pref_size (this)
		; (. button :pref_size) -> (width height)
		(defq border (abs (get :border this)))
		(bind '(w h) (. (get :label_flow this) :pref_size))
		(list (+ (* border 2) w) (+ (* border 2) h)))

	(method :layout (this)
		; (. button :layout) -> button
		(defq flow (get :label_flow this) state (get :state this)
			border (get :border this) pos border)

		;contents offset or not
		(if (/= 0 state)
			(setq pos (* pos 2)))
		(bind '(w h) (view-get-size this))
		(view-change flow pos pos (- w (* border 2)) (- h (* border 2)))
		(view-layout flow)
		(view-set-flags this view_flag_opaque view_flag_opaque)
		this)

	(method :mouse_down (this event)
		; (. button :mouse_down event) -> button
		(def this :state 1)
		(. this :layout)
		(view-dirty-all this))

	(method :mouse_up (this event)
		; (. button :mouse_up event) -> button
		(when (/= (get :state this) 0)
			(def this :state 0)
			(. this :layout)
			(view-dirty-all this)
			(component-emit this))
		this)

	(method :mouse_move (this event)
		; (. button :mouse_move event) -> button
		(defq rx (get-int event ev_msg_mouse_rx) ry (get-int event ev_msg_mouse_ry))
		(bind '(w h) (view-get-size this))
		(if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h))
			(defq state 1)
			(defq state 0))
		(when (/= state (get :state this))
			(def this :state state)
			(. this :layout)
			(view-dirty-all this))
		this)
	)
