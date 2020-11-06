;;;;;;;;;;;;;;;
; button widget
;;;;;;;;;;;;;;;

(ffi create-button "gui/button/lisp_create" 0)

(class button ()
	; (button) -> button
	; overide the default 'this' env with a button component
	(defq this (create-button))

	(method :layout (this)
		; (. button :layout) -> button
		(defq flow (get-field this (const label_flow) 0)
			state (get-field this (const button_state) (const int_size))
			border (get :border this) pos border)

		;contents offset or not
		(if (/= 0 (logand state button_state_pressed))
			(setq pos (* pos 2)))
		(bind '(w h) (view-get-size this))
		(view-change flow pos pos (- w (* border 2)) (- h (* border 2)))
		(view-layout flow)
		(view-set-flags this view_flag_opaque view_flag_opaque)
		this)
	)
