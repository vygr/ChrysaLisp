;;;;;;;;;;;;;;;
; label widget
;;;;;;;;;;;;;;;

(defclass Label ()
	; (Label) -> label
	; overide the default 'this' env with a label component
	; inherit from View !
	(def (defq label_flow (Flow) label_text (Text) this (View))
		:label_flow label_flow :label_text label_text)
	(. label_flow :set_flags 0 -1)
	(. this :add_back label_flow)
	(. label_flow :add_child label_text)

	;draw function for compositor...
	(defiimethod :draw "gui/label/draw")

	(defmethod :add_child (this child)
		; (. label :add_child child) -> label
		(. (get :label_flow this) :add_child child)
		this)

	(defmethod :pref_size (this)
		; (. label :pref_size) -> (width height)
		(defq mw (get :min_width this) mh (get :min_height this)
			border (abs (get :border this)))
		(setd mw 0 mh 0)
		(bind '(w h) (. (get :label_flow this) :pref_size))
		(list (+ (* border 2) (max w mw)) (+ (* border 2) (max h mh))))

	(defmethod :layout (this)
		; (. label :layout) -> label
		(defq border (abs (get :border this)))
		(bind '(w h) (. this :get_size))
		(. (view-change (get :label_flow this) border border (- w (* border 2)) (- h (* border 2))) :layout)
		(if (= (>> (get :color this) 24) 0xff)
			(. this :set_flags +view_flag_opaque+ +view_flag_opaque+)
			this))
	)
