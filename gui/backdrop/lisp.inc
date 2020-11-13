;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(class Backdrop ()
	; (Backdrop)-> backdrop
	; overide the default 'this' env with a backdrop component
	; inherit from View !
	(defq this (View))

	;draw function for compositor...
	(def this :draw (ffi _ "gui/backdrop/draw" 0))
	)
