;;;;;;;;;;;;;;;;;
; slider widget
;;;;;;;;;;;;;;;;;

(ffi create-slider "gui/slider/lisp_create" 0)

(class Slider ()
	; (Slider) -> slider
	; overide the default 'this' env with a slider component
	(defq this (create-slider))

	(method :pref_size (this)
		; (. slider :pref_size) -> (width height)
		(list 10 10))

	(method :layout (this)
		; (. slider :layout) -> slider
		(if (= (>> (get :color this) 24) 0xff)
			(view-set-flags this view_flag_opaque view_flag_opaque))
		this)
	)
