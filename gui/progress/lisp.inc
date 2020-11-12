;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(class Progress ()
	; (Progress) -> progress
	; overide the default 'this' env with a progress component
	; inherit from View !
	(defq this ((ffi _ "gui/progress/lisp_create" 0)))
	(each (lambda ((key val)) (def this key val)) (tolist (View)))

	(method :pref_size (this)
		; (. progress :pref_size) -> (width height)
		(list 256 10))
	)
