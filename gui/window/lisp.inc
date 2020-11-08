;;;;;;;;;;;;;;;
; window widget
;;;;;;;;;;;;;;;

(ffi create-window "gui/window/lisp_create" 0)

(class Window ()
	; (Window) -> window
	; overide the default 'this' env with a window component
	(defq this (create-window))

	(method :layout (this)
		; (. window :layout) -> window
		(defq border (get :border this) shadow (get :shadow this) color (get :color this)
			child (get-field this (const window_child) 0))
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
