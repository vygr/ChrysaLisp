;;;;;;;;;;;;;
; text widget
;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "gui/font/lisp.inc")
(import "gui/texture/lisp.inc")

(defclass Text ()
	; (Text) -> text
	; overide the default 'this' env with a text component
	; inherit from View !
	(def (defq this (View)) :color 0)

	(defmethod :draw (this)
		; (. text :draw) -> text
		(when (and (defq font (get :font this)) (defq text (get :text this))
				(defq texture (font-sym-texture font (sym text))))
			(bind '(tid tw th) (texture-metrics texture))
			(bind '(w h) (. this :get_size))
			(. this :ctx_blit tid (get :ink_color this) 0 (>>> (- h th) 1) tw th))
		this)

	(defmethod :pref_size (this)
		; (. text :pref_size) -> (width height)
		(defq font (get :font this) text (get :text this) w 0 h 0)
		(when (and font text)
			(bind '(w h) (font-glyph-bounds font text)))
		(list w h))
	)
