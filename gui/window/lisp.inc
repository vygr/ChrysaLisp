;;;;;;;;;;;;;;;
; window widget
;;;;;;;;;;;;;;;

(ffi create-window "gui/window/lisp_create" 0)

(class Window ()
	; (Window) -> window
	; overide the default 'this' env with a window component
	(def (defq this (create-window)) :child nil)

	(method :add_child (this child)
		; (. window :add_child child) -> this
		(if (defq old_child (get :child this))
			(view-sub old_child))
		(set this :child child)
		(view-add-back this child)
		this)

	(method :pref_size (this)
		; (. window :pref_size) -> (width height)
		(defq child (get :child this) border (get :border this) shadow
			(get :shadow this) w 0 h 0)
		(if child (bind '(w h) (view-pref-size child)))
		(list (+ (* (+ shadow border) 2) w) (+ (* (+ shadow border) 2) h)))

	(method :layout (this)
		; (. window :layout) -> window
		(defq border (get :border this) shadow (get :shadow this)
			color (get :color this) child (get :child this))
		;position any child
		(bind '(w h) (view-get-size this))
		(when child
			(view-change child (+ border shadow) (+ border shadow)
				(- w (* 2 (+ border shadow))) (- h (* 2 (+ border shadow)))))

		;adjust window transparetncy details based on color and shadow
		(when (= (>> (get :color this) 24) 0xff)
			(cond
				((= shadow 0)
					(view-set-flags this view_flag_opaque view_flag_opaque))
				(t
					(view-clr-opaque this)
					(view-add-opaque this (+ border shadow) (+ border shadow)
						(- w (* 2 (+ border shadow))) (- h (* 2 (+ border shadow)))))))
		this)
	)
