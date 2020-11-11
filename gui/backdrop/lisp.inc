;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(ffi create-backdrop "gui/backdrop/lisp_create" 0)

(class Backdrop ()
	; (Backdrop)-> backdrop
	; overide the default 'this' env with a backdrop component
	(defq this (create-backdrop))

	(method :layout (this)
		; (. backdrop :layout) -> backdrop
		(if (= (>> (get :color this) 24) 0xff)
			(view-set-flags this +view_flag_opaque+ +view_flag_opaque+))
		this)
	)
