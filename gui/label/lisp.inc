;;;;;;;;;;;;;;;
; label widget
;;;;;;;;;;;;;;;

(import "gui/flow/lisp.inc")
(import "gui/text/lisp.inc")

(defclass Label () (View)
	; (Label) -> label
	(defq label_flow (Flow) label_text (Text))
	(def this :label_flow label_flow :label_text label_text)
	(.-> label_flow (:set_flags 0 -1) (:add_child label_text))
	(. this :add_back label_flow)

	(defmethod :draw (this)
		; (. label :draw) -> label
		(bind '(w h) (. this :get_size))
		(. this :ctx_panel (get :color this) 1 (get :border this) 0 0 w h))

	(defmethod :add_child (this child)
		; (. label :add_child child) -> label
		(. (get :label_flow this) :add_child child)
		this)

	(defmethod :pref_size (this)
		; (. label :pref_size) -> (width height)
		(defq mw (def? :min_width this) mh (def? :min_height this)
			border (abs (get :border this)))
		(setd mw 0 mh 0)
		(bind '(w h) (. (get :label_flow this) :pref_size))
		(list (+ (* border 2) (max w mw)) (+ (* border 2) (max h mh))))

	(defmethod :layout (this)
		; (. label :layout) -> label
		(defq border (abs (get :border this)))
		(bind '(w h) (. this :get_size))
		(.-> (get :label_flow this) (:change border border (- w (* border 2)) (- h (* border 2))) :layout)
		(if (= (>> (get :color this) 24) 0xff)
			(. this :set_flags +view_flag_opaque +view_flag_opaque)
			this))
	)
