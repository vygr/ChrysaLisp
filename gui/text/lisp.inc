;;;;;;;;;;;;;
; text widget
;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "gui/font/lisp.inc")
(import "gui/texture/lisp.inc")

(defclass Text () (View)
	; (Text) -> text
	(def this :color 0 :sym_cache :nil :text_cache :nil)

	(defmethod :draw (this)
		; (. text :draw) -> text
		(when (and (defq font (get :font this)) (defq text (get :text this))
				(if (eql (def? :text_cache this) text)
					(defq texture (font-sym-texture font (get :sym_cache this)))
					(defq texture (font-sym-texture font (set this :text_cache text :sym_cache (sym text))))))
			(bind '(tid tw th) (texture-metrics texture))
			(bind '(w h) (. this :get_size))
			(. this :ctx_blit tid (get :ink_color this) 0 (>>> (- h th) 1) tw th))
		this)

	(defmethod :pref_size (this)
		; (. text :pref_size) -> (width height)
		(defq font (get :font this) text (get :text this) w 0 h 0)
		(and font text (bind '(w h) (font-glyph-bounds font text)))
		(list w h))
	)
