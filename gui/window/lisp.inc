;;;;;;;;;;;;;;;
; window widget
;;;;;;;;;;;;;;;

(import "gui/scroll/lisp.inc")

(defclass Window () (View)
	; (Window) -> window
	(def this :child nil :last_buttons 0 :is_window t
		:drag_mode 0 :drag_offx 0 :drag_offy 0)

	(defmethod :draw (this)
		; (. window :draw) -> window
		(bind '(w h) (. this :get_size))
		;panel
		(. this :ctx_panel (get :color this) 1 (get :border this)
			(defq shadow (get :shadow this)) shadow (- w (* shadow 2)) (- h (* shadow 2)))
		;shadow
		(defq col 0x80000000)
		(while (and (> shadow 0) (/= col 0))
			(.-> this (:ctx_set_color col) (:ctx_box shadow shadow (- w (* shadow 2)) (- h (* shadow 2))))
			(setq col (logand 0xff000000 (+ (>> col 1) (>> col 4))) shadow (dec shadow)))
		this)

	(defmethod :add_child (this child)
		; (. window :add_child child) -> window
		(if (defq old_child (get :child this))
			(. old_child :sub))
		(lower :child)
		(. this :add_back child))

	(defmethod :pref_size (this)
		; (. window :pref_size) -> (width height)
		(raise :border :shadow :child)
		(bind '(w h) (. child :pref_size))
		(list (+ (* (+ shadow border) 2) w) (+ (* (+ shadow border) 2) h)))

	(defmethod :layout (this)
		; (. window :layout) -> window
		(raise :border :shadow :child)
		;position any child
		(bind '(w h) (. this :get_size))
		(when child
			(. child :change (+ border shadow) (+ border shadow)
				(- w (* 2 (+ border shadow))) (- h (* 2 (+ border shadow)))))
		;adjust window transparetncy details based on color and shadow
		(when (= (>> (get :color this) 24) 0xff)
			(cond
				((= shadow 0)
					(. this :set_flags +view_flag_opaque +view_flag_opaque))
				(t	(.-> this :clr_opaque (:add_opaque (+ border shadow) (+ border shadow)
						(- w (* 2 (+ border shadow))) (- h (* 2 (+ border shadow)))))))))

	(defmethod :drag_mode (this rx ry)
		; (. window :drag_mode rx ry) -> (drag_mode drag_offx drag_offy)
		(bind '(w h) (. this :get_size))
		(raise :border :shadow (drag_mode 0 drag_offx 0 drag_offy 0))
		(if (< rx (+ border shadow))
			(setq drag_mode (+ drag_mode 1) drag_offx rx))
		(if (< ry (+ border shadow))
			(setq drag_mode (+ drag_mode 2) drag_offy ry))
		(if (>= rx (- w border shadow))
			(setq drag_mode (+ drag_mode 4) drag_offx (- rx w)))
		(if (>= ry (- h border shadow))
			(setq drag_mode (+ drag_mode 8) drag_offy (- ry h)))
		(list drag_mode drag_offx drag_offy))

	(defmethod :mouse_down (this event)
		; (. window :mouse_down event) -> window
		(bind '(drag_mode drag_offx drag_offy)
			(. this :drag_mode (getf event +ev_msg_mouse_rx) (getf event +ev_msg_mouse_ry)))
		(lower :drag_mode :drag_offx :drag_offy)
		this)

	(defmethod :mouse_move (this event)
		; (. window :mouse_move event) -> window
		(raise :drag_mode :drag_offx :drag_offy
			(ax (getf event +ev_msg_mouse_x) ay (getf event +ev_msg_mouse_y)))
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
		(.-> this (:change_dirty x y (- x1 x) (- y1 y)) :emit))

	(defmethod :event (this event)
		; (. window :event event) -> window
		(defq target (. this :find_id (getf event +ev_msg_target_id))
			type (getf event +ev_msg_type))
		(when target
			(cond
				((= type +ev_type_mouse)
					;so what state are we in ?
					(defq buttons (getf event +ev_msg_mouse_buttons))
					(cond
						((/= 0 (get :last_buttons this))
							;was down previously
							(cond
								((/= 0 buttons)
									;is down now, so move
									(if (defq fnc (.? target :mouse_move)) (fnc target event)))
								(t  ;is not down now, so release
									(set this :last_buttons 0)
									(if (defq fnc (.? target :mouse_up)) (fnc target event)))))
						(t  ;was not down previously
							(cond
								((/= 0 buttons)
									;is down now, so first down
									(set this :last_buttons buttons)
									(if (defq fnc (.? target :mouse_down)) (fnc target event)))
								(t  ;is not down now, so hover
									(if (defq fnc (.? target :mouse_hover)) (fnc target event)))))))
				((= type +ev_type_key)
					(if (>= (getf event +ev_msg_key_keycode) 0)
						(if (defq fnc (.? target :key_down)) (fnc target event))
						(if (defq fnc (.? target :key_up)) (fnc target event))))
				((= type +ev_type_wheel)
					(while (and target (not (defq fnc (.? target :mouse_wheel))))
						(setq target (penv target)))
					(if target (fnc target event)))
				((= type +ev_type_enter)
					(if (defq fnc (.? target :mouse_enter)) (fnc target event)))
				((= type +ev_type_exit)
					(if (defq fnc (.? target :mouse_exit)) (fnc target event)))
				((= type +ev_type_action)
					(if (defq fnc (.? target :action)) (fnc target event)))))
		this)
	)
