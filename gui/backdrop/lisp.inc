;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(class Backdrop ()
	; (Backdrop)-> backdrop
	; overide the default 'this' env with a backdrop component
	; inherit from View !
	(defq this (View))

	;draw function for compositor...
	(ffi-method :draw "gui/backdrop/draw")
	)
