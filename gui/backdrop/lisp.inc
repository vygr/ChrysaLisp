;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(defclass Backdrop ()
	; (Backdrop)-> backdrop
	; overide the default 'this' env with a backdrop component
	; inherit from View !
	(defq this (View))

	;draw function for compositor...
	(defiimethod :draw "gui/backdrop/draw")
	)
