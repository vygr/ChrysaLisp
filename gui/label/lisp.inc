;;;;;;;;;;;;;;;
; label widget
;;;;;;;;;;;;;;;

(import "././flow/lisp.inc")
(import "././text/lisp.inc")

(defclass Label () (View)
	; (Label) -> label
	(defq label_flow (Flow) label_text (Text))
	(def this :label_flow label_flow :label_text label_text)
	(.-> label_flow (:set_flags 0 -1) (:add_child label_text))
	(. this :add_back label_flow)

	(defmethod :draw ()
		; (. label :draw) -> label
		(bind '(w h) (. this :get_size))
		(. this :ctx_panel (get :color this) 1 (get :border this) 0 0 w h))

	(defmethod :add_child (child)
		; (. label :add_child child) -> label
		(. (get :label_flow this) :add_child child)
		this)

	(defmethod :constraint ()
		; (. label :constraint) -> (width height)
		(defq border (abs (get :border this)))
		(bind '(w h) (. (get :label_flow this) :get_constraint))
		(list (+ (* border 2) (max w (ifn (def? :min_width this) 0)))
			(+ (* border 2) (max h (ifn (def? :min_height this) 0)))))

	(defmethod :layout ()
		; (. label :layout) -> label
		(defq border (abs (get :border this)))
		(bind '(w h) (. this :get_size))
		(.-> (get :label_flow this)
			(:set_bounds border border (- w (* border 2)) (- h (* border 2)))
			(:constrain :t))
		(if (= (>> (get :color this) 24) 0xff)
			(. this :set_flags +view_flag_opaque +view_flag_opaque)
			this))
	)
