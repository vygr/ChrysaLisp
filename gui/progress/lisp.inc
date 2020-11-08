;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(ffi create-progress "gui/progress/lisp_create" 0)

(class Progress ()
	; (Progress) -> progress
	; overide the default 'this' env with a progress component
	(defq this (create-progress))

	(method :pref_size (this)
		; (. progress :pref_size) -> (width height)
		(list 256 10))

	(method :layout (this)
		; (. progress :layout) -> progress
		(if (= (>> (get :color this) 24) 0xff)
			(view-set-flags this view_flag_opaque view_flag_opaque))
		this)
	)
