;;;;;;;;;;;;;;;;;
; slider widget
;;;;;;;;;;;;;;;;;

(class Slider ()
	; (Slider) -> slider
	; overide the default 'this' env with a slider component
	; inherit from View !
	(def (defq this (View)) :state 0 :value 0 :maximum 0 :portion 0 :downxy 0)

	;draw function for compositor...
	(def this :draw (ffi _ "gui/slider/draw" 0))

	(method :pref_size (this)
		; (. slider :pref_size) -> (width height)
		(list 10 10))

	(method :mouse_down (this event)
		; (. slider :mouse_down event) -> slider
		(bind '(w h) (. this :get_size))
		(defq rx (get-int event ev_msg_mouse_rx) ry (get-int event ev_msg_mouse_ry))
		(def this :old_value (get :value this) :downxy (if (> w h) rx ry) :state 1)
		(view-dirty-all this)
		(. this :mouse_move event))

	(method :mouse_up (this event)
		; (. slider :mouse_up event) -> slider
		(when (/= (get :state this) 0)
			(def this :state 0)
			(view-dirty-all this))
		this)

	(method :mouse_move (this event)
		; (. slider :mouse_move event) -> slider
		(bind '(w h) (. this :get_size))
		(defq rx (get-int event ev_msg_mouse_rx) ry (get-int event ev_msg_mouse_ry)
			downxy (get :downxy this) maximum (get :maximum this) portion (get :portion this)
			state (if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) 1 0))
		(if (> w h)
			(defq new_value (/ (* (- rx downxy) (+ maximum portion)) w))
			(defq new_value (/ (* (- ry downxy) (+ maximum portion)) h)))
		(setq new_value (max 0 (min maximum (+ new_value (get :old_value this)))))
		(when (/= new_value (get :value this))
			(def this :value new_value)
			(component-emit this)
			(view-dirty-all this))
		(when (/= state (get :state this))
			(def this :state state)
			(view-dirty-all this))
		this)
	)
