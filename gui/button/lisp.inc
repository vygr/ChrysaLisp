;;;;;;;;;;;;;;;
; button widget
;;;;;;;;;;;;;;;

(import "././label/lisp.inc")

(defclass Button () (Label)
	; (Button) -> button
	(def this :state 1)

	(defmethod :draw ()
		; (. button :draw) -> button
		(bind '(w h) (. this :get_size))
		(. this :ctx_panel (get :color this) 1 (* (get :border this) (get :state this)) 0 0 w h))

	(defmethod :layout ()
		; (. button :layout) -> button
		(defq flow (get :label_flow this) state (get :state this)
			border (get :border this) pos border)
		;contents offset or not
		(if (/= 1 state)
			(setq pos (* pos 2)))
		(bind '(w h) (. this :get_size))
		(. flow :set_bounds pos pos (- w (* border 2)) (- h (* border 2)))
		(. this :set_flags +view_flag_opaque +view_flag_opaque))

	(defmethod :mouse_down (event)
		; (. button :mouse_down event) -> button
		(def (. this :mouse_exit event) :state -1)
		(.-> this :layout :dirty_all))

	(defmethod :mouse_up (event)
		; (. button :mouse_up event) -> button
		(when (/= (get :state this) 1)
			(def this :state 1)
			(.-> this :layout :dirty_all :emit))
		this)

	(defmethod :mouse_move (event)
		; (. button :mouse_move event) -> button
		(bind '(w h) (. this :get_size))
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry)
			state (if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) -1 1))
		(when (/= state (get :state this))
			(def this :state state)
			(.-> this :layout :dirty_all))
		this)

	(defmethod :mouse_enter (event)
		; (. button :mouse_enter event) -> button
		(when (and (not (def? :tip_window this))
				(def? :tip_text this)
				(defq tip_mbox (get :tip_mbox this)))
			(mail-timeout tip_mbox 1000000 (. this :get_id)))
		this)

	(defmethod :mouse_exit (event)
		; (. button :mouse_exit event) -> button
		(if (defq tip_mbox (get :tip_mbox this))
			(mail-timeout tip_mbox 0 0))
		(when (defq tip_window (def? :tip_window this))
			(gui-sub-rpc tip_window)
			(undef this :tip_window))
		this)

	(defmethod :show_tip ()
		; (. button :show_tip) -> button
		(when (and (not (def? :tip_window this))
				(defq tip_text (def? :tip_text this)))
			(def (defq tip_window (Node))
				:text tip_text
				:color +argb_white
				:font *env_tip_font*)
			(. tip_window :set_flags 0 +view_flag_solid)
			(bind '(x y w h) (apply view-locate (push (. tip_window :pref_size) :bottom)))
			(gui-add-front-rpc (. tip_window :change x y w h))
			(def this :tip_window tip_window))
		this)
	)
