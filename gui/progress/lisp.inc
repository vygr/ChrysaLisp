;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")

(defclass Progress ()
	; (Progress) -> progress
	; overide the default 'this' env with a progress component
	; inherit from View !
	(defq this (View))

	;draw function for compositor...
	(deffimethod :draw "gui/progress/draw")

	(defmethod :pref_size (this)
		; (. progress :pref_size) -> (width height)
		(list 256 10))
	)
