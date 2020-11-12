;;;;;;;;;;;;;;;
; label widget
;;;;;;;;;;;;;;;

(class Label ()
	; (Label) -> label
	; overide the default 'this' env with a label component
	; inherit from View !
	(def (defq label_flow (Flow) label_text (create-text) this ((ffi _ "gui/label/lisp_create" 0)))
		:label_flow label_flow :label_text label_text)
	(each (lambda ((key val)) (def this key val)) (tolist (View)))
	(view-set-flags label_flow 0 -1)
	(view-add-back this label_flow)
	(. label_flow :add_child label_text)

	(method :add_child (this child)
		; (. label :add_child child) -> label
		(. (get :label_flow this) :add_child child)
		this)

	(method :pref_size (this)
		; (. label :pref_size) -> (width height)
		(defq flow (get :label_flow this) border (abs (get :border this))
			mw (get :min_width this) mh (get :min_height this))
		(setd mw 0 mh 0)
		(bind '(w h) (. flow :pref_size))
		(list (+ (* border 2) (max w mw)) (+ (* border 2) (max h mh))))

	(method :layout (this)
		; (. label :layout) -> label
		(defq flow (get :label_flow this) border (abs (get :border this))
			color (get :color this))
		(bind '(w h) (. this :get_size))
		(view-change flow border border (- w (* border 2)) (- h (* border 2)))
		(. flow :layout)
		(if (= (>> color 24) 0xff)
			(view-set-flags this +view_flag_opaque+ +view_flag_opaque+))
		this)
	)
