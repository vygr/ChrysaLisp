;;;;;;;;;;;;;
; text widget
;;;;;;;;;;;;;

(class Text ()
	; (Text) -> text
	; overide the default 'this' env with a text component
	; inherit from View !
	(def (defq this (View)) :color 0)

	;draw function for compositor...
	(ffi-method :draw "gui/text/draw")

	(method :pref_size (this)
		; (. text :pref_size) -> (width height)
		(defq font (get :font this) text (get :text this) w 0 h 0)
		(when (and font text)
			(bind '(w h) (font-glyph-bounds font text)))
		(list w h))
	)
