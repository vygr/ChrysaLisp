;;;;;;;;;;;;;;;;;;
; title widget
;;;;;;;;;;;;;;;;;;

(ffi create-title "gui/title/lisp_create" 0)

(class title ()
	; (title) -> title
	; overide the default 'this' env with a title component
	(defq this (create-title))

	(method :layout (this)
		; (. title :layout) -> title
		(defq flow (get-field this (const label_flow) 0)
			border (abs (get :border this)) color (get :color this))
		(bind '(w h) (view-get-size this))
		(view-change flow border border (- w (* border 2)) (- h (* border 2)))
		(view-layout flow)
		(if (= (>> color 24) 0xff)
			(view-set-flags this view_flag_opaque view_flag_opaque))
		this)
	)
