;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(ffi create-progress "gui/progress/lisp_create" 0)

(class progress ()
	; (progress) -> progress
	; overide the default 'this' env with a progress component
	(defq this (create-progress))

	(method :layout (this)
		; (. progress :layout) -> progress
		(if (= (>> (get :color this) 24) 0xff)
			(view-set-flags this view_flag_opaque view_flag_opaque))
		this)
	)
