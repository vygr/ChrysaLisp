;;;;;;;;;;;;;;;;;
; slider widget
;;;;;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defclass Slider () (View)
	; (Slider) -> slider
	(def this :state 0 :value 0 :maximum 0 :portion 0 :downxy 0)

	(defmethod :draw ()
		; (. slider :draw) -> slider
		(defq maximum (get :maximum this) portion (get :portion this) col (get :color this))
		;a trough, a shade of the color with a thin edge a shade darker,
		;and no black line
		(bind '(w h) (. this :get_size))
		(.-> this (:ctx_set_color (canvas-darker (defq trough (canvas-darker col)))) (:ctx_box 0 0 w h)
			(:ctx_set_color trough) (:ctx_filled_box 1 1 (- w 2) (- h 2)))
		;elevator, the color, a pixel clear of the edge all round
		(. this :ctx_set_color (if (= (get :state this) 0) col (canvas-brighter col)))
		(cond
			((> w h)
				(-- w 2)
				(cond
					((> maximum 0)
						(setq portion (max (- h 2) (/ (* w portion) (+ maximum portion)))
							w (/ (* (get :value this) (- w portion)) maximum)))
					(:t (setq portion w w 0)))
				(. this :ctx_filled_box (+ w 2) 2 (- portion 2) (- h 4)))
			(:t (-- h 2)
				(cond
					((> maximum 0)
						(setq portion (max (- w 2) (/ (* h portion) (+ maximum portion)))
							h (/ (* (get :value this) (- h portion)) maximum)))
					(:t (setq portion h h 0)))
				(. this :ctx_filled_box 2 (+ h 2) (- w 4) (- portion 2)))))

	(defmethod :constraint ()
		; (. slider :constraint) -> (width height)
		(list (ifn (def? :min_width this) 10) (ifn (def? :min_height this) 10)))

	(defmethod :mouse_down (event)
		; (. slider :mouse_down event) -> slider
		(bind '(w h) (. this :get_size))
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry))
		(def this :old_value (get :value this) :downxy (if (> w h) rx ry) :state 1)
		(.-> this :dirty_all (:mouse_move event)))

	(defmethod :mouse_up (event)
		; (. slider :mouse_up event) -> slider
		(when (/= (get :state this) 0)
			(def this :state 0)
			(. this :dirty_all))
		this)

	(defmethod :mouse_move (event)
		; (. slider :mouse_move event) -> slider
		(bind '(w h) (. this :get_size))
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry)
			downxy (get :downxy this) maximum (get :maximum this) portion (get :portion this)
			state (if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) 1 0)
			new_value (if (> w h)
				(/ (* (- rx downxy) (+ maximum portion)) w)
				(/ (* (- ry downxy) (+ maximum portion)) h)))
		(setq new_value (max 0 (min maximum (+ new_value (get :old_value this)))))
		(when (/= new_value (get :value this))
			(def this :value new_value)
			(.-> this :emit :dirty_all))
		(when (/= state (get :state this))
			(def this :state state)
			(. this :dirty_all))
		this)
	)
