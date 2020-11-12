;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(class Backdrop ()
	; (Backdrop)-> backdrop
	; overide the default 'this' env with a backdrop component
	; inherit from View !
	(defq this ((ffi _ "gui/backdrop/lisp_create" 0)))
	(each (lambda ((key val)) (def this key val)) (tolist (View)))
	)
