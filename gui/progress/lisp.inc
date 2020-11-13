;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(class Progress ()
	; (Progress) -> progress
	; overide the default 'this' env with a progress component
	; inherit from View !
	(defq this (View))

	;draw function for compositor...
	(def this :draw (ffi _ "gui/progress/draw" 0))

	(method :pref_size (this)
		; (. progress :pref_size) -> (width height)
		(list 256 10))
	)
