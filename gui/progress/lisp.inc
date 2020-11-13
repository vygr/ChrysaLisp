;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(class Progress ()
	; (Progress) -> progress
	; overide the default 'this' env with a progress component
	; inherit from View !
	(defq this (View))

	;draw function for compositor...
	(ffi-method :draw "gui/progress/draw")

	(method :pref_size (this)
		; (. progress :pref_size) -> (width height)
		(list 256 10))
	)
