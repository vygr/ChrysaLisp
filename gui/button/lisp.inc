;;;;;;;;;;;;;;;
; button widget
;;;;;;;;;;;;;;;

(ffi create-button "gui/button/lisp_create" 0)

(class Button ()
	; (Button) -> button
	; overide the default 'this' env with a button component
	(defq this (create-button) flow (Flow) text (create-text))
	(set-field this (const label_flow) 0 flow)
	(set-field this (const label_text) 0 text)
	(view-set-flags flow 0 -1)
	(view-add-child this flow)
	(view-add-child flow text)

	(method :pref_size (this)
		; (. button :pref_size) -> (width height)
		(defq flow (get-field this (const label_flow) 0) border (abs (get :border this))
			w 0 h 0)
		(if flow (bind '(w h) (view-pref-size flow)))
		(list (+ (* border 2) w) (+ (* border 2) h)))

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
