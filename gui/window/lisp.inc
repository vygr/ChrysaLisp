;;;;;;;;;;;;;;;
; window widget
;;;;;;;;;;;;;;;

(class Window ()
	; (Window) -> window
	; overide the default 'this' env with a window component
	; inherit from View !
	(def (defq this (View)) :child nil :last_buttons 0 :is_window t)

	;draw function for compositor...
	(def this :draw (ffi _ "gui/window/draw" 0))

	(method :add_child (this child)
		; (. window :add_child child) -> window
		(if (defq old_child (get :child this))
			(. old_child :sub))
		(set this :child child)
		(. this :add_back child))

	(method :pref_size (this)
		; (. window :pref_size) -> (width height)
		(defq child (get :child this) border (get :border this) shadow
			(get :shadow this))
		(bind '(w h) (. child :pref_size))
		(list (+ (* (+ shadow border) 2) w) (+ (* (+ shadow border) 2) h)))

	(method :layout (this)
		; (. window :layout) -> window
		(defq border (get :border this) shadow (get :shadow this)
			color (get :color this) child (get :child this))
		;position any child
		(bind '(w h) (. this :get_size))
		(when child
			(view-change child (+ border shadow) (+ border shadow)
				(- w (* 2 (+ border shadow))) (- h (* 2 (+ border shadow)))))

		;adjust window transparetncy details based on color and shadow
		(when (= (>> (get :color this) 24) 0xff)
			(cond
				((= shadow 0)
					(. this :set_flags +view_flag_opaque+ +view_flag_opaque+))
				(t
					(. this :clr_opaque)
					(. this :add_opaque (+ border shadow) (+ border shadow)
						(- w (* 2 (+ border shadow))) (- h (* 2 (+ border shadow)))))))
		this)

	(method :mouse_down (this event)
		; (. window :mouse_down event) -> window
		(bind '(w h) (. this :get_size))
		(defq rx (get-int event ev_msg_mouse_rx) ry (get-int event ev_msg_mouse_ry)
			border (get :border this) shadow (get :shadow this)
			drag_mode 0 drag_offx 0 drag_offy 0)
		(if (< rx (+ border shadow))
			(setq drag_mode (+ drag_mode 1) drag_offx rx))
		(if (< ry (+ border shadow))
			(setq drag_mode (+ drag_mode 2) drag_offy ry))
		(if (>= rx (- w border shadow))
			(setq drag_mode (+ drag_mode 4) drag_offx (- rx w)))
		(if (>= ry (- h border shadow))
			(setq drag_mode (+ drag_mode 8) drag_offy (- ry h)))
		(def this :drag_mode drag_mode :drag_offx drag_offx :drag_offy drag_offy)
		this)

	(method :mouse_move (this event)
		; (. window :mouse_move event) -> window
		(defq ax (get-int event ev_msg_mouse_x) ay (get-int event ev_msg_mouse_y)
			drag_mode (get :drag_mode this) drag_offx (get :drag_offx this)
			drag_offy (get :drag_offy this))
		(bind '(x y x1 y1) (. this :get_bounds))
		(bind '(w h) (. this :pref_size))
		(setq x1 (+ x x1) y1 (+ y y1))
		(if (/= 0 (logand 1 drag_mode))
			(setq x (min (- ax drag_offx) (- x1 w))))
		(if (/= 0 (logand 2 drag_mode))
			(setq y (min (- ay drag_offy) (- y1 h))))
		(if (/= 0 (logand 4 drag_mode))
			(setq x1 (max (- ax drag_offx) (+ x w))))
		(if (/= 0 (logand 8 drag_mode))
			(setq y1 (max (- ay drag_offy) (+ y h))))
		(. this :change_dirty x y (- x1 x) (- y1 y))
		(component-emit this)
		this)

	(method :event (this event)
		; (. window :event event) -> window
		(defq target (. this :find_id (get-long event ev_msg_target_id))
			type (get-long event ev_msg_type))
		(when target
			(cond
				((= type (const ev_type_mouse))
					;so what state are we in ?
					(defq buttons (get-uint event (const ev_msg_mouse_buttons)))
					(cond
						((/= 0 (get :last_buttons this))
							;was down previously
							(cond
								((/= 0 buttons)
									;is down now, so move
									(if (get :mouse_move target) (. target :mouse_move event)))
								(t	;is not down now, so release
									(set this :last_buttons 0)
									(if (get :mouse_up target) (. target :mouse_up event)))))
						(t	;was not down previously
							(cond
								((/= 0 buttons)
									;is down now, so first down
									(set this :last_buttons buttons)
									(if (get :mouse_down target) (. target :mouse_down event)))
								(t	;is not down now, so hover
									(if (get :mouse_hover target) (. target :mouse_hover event)))))))
				((= type (const ev_type_key))
					(if (>= (get-int event (const ev_msg_key_keycode)) 0)
						(if (get :key_down target) (. target :key_down event))
						(if (get :key_up target) (. target :key_up event))))
				((= type (const ev_type_action))
					(if (get :action target) (. target :action event)))))
		this)
	)
