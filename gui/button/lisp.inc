;;;;;;;;;;;;;;;
; button widget
;;;;;;;;;;;;;;;

(class Button ()
	; (Button) -> button
	; overide the default 'this' env with a button component
	; inherit from Label !
	(def (defq this (Label)) :state 0)

	;draw function for compositor...
	(def this :draw (ffi _ "gui/button/draw" 0))

	(method :layout (this)
		; (. button :layout) -> button
		(defq flow (get :label_flow this) state (get :state this)
			border (get :border this) pos border)

		;contents offset or not
		(if (/= 0 state)
			(setq pos (* pos 2)))
		(bind '(w h) (. this :get_size))
		(view-change flow pos pos (- w (* border 2)) (- h (* border 2)))
		(. flow :layout)
		(. this :set_flags +view_flag_opaque+ +view_flag_opaque+)
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
		(bind '(w h) (. this :get_size))
		(defq rx (get-int event ev_msg_mouse_rx) ry (get-int event ev_msg_mouse_ry)
			state (if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) 1 0))
		(when (/= state (get :state this))
			(def this :state state)
			(. this :layout)
			(view-dirty-all this))
		this)
	)
