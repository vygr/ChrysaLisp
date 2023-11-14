;;;;;;;;;;;;;
; text widget
;;;;;;;;;;;;;

(import "././view/lisp.inc")
(import "././font/lisp.inc")
(import "././texture/lisp.inc")

(defclass Text () (View)
	; (Text) -> text
	(def this :color 0 :sym_cache :nil :text_cache :nil :offset 0)

	(defmethod :draw ()
		; (. text :draw) -> text
		(raise :font :text :offset :sym_cache)
		(when (and font text (if (eql (def? :text_cache this) text)
				(defq texture (font-sym-texture font sym_cache))
				(defq texture (font-sym-texture font (set this :text_cache text :sym_cache (sym text))))))
			(bind '(tid tw th) (texture-metrics texture))
			(bind '(w h) (. this :get_size))
			(. this :ctx_blit tid (get :ink_color this) offset (>>> (- h th) 1) tw th))
		this)

	(defmethod :pref_size ()
		; (. text :pref_size) -> (width height)
		(defq font (get :font this) text (get :text this)
			mw (def? :min_width this) mh (def? :min_height this)
			w 0 h 0)
		(setd mw 0 mh 0)
		(and font text (bind '(w h) (font-glyph-bounds font text)))
		(list (max w mw) (max h mh)))
	)
