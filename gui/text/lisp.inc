;;;;;;;;;;;;;
; text widget
;;;;;;;;;;;;;

(import "././view/lisp.inc")
(import "././font/lisp.inc")
(import "././texture/lisp.inc")
(import "././canvas/lisp.inc")

;state flags
(bits +text_state 0
	(bit pressed selected highlight))

(defclass Text () (View)
	; (Text) -> text
	(def this :color 0 :state 0
		:sym_cache :nil :text_cache :nil :offset 0)

	(defmethod :draw ()
		; (. text :draw) -> text
		(raise :font :text :offset :sym_cache :state :color)
		(bind '(w h) (. this :get_size))
		(if (/= (logand state +text_state_highlight) 0)
			(setq color *env_highlight_col*))
		(if (/= (logand state +text_state_selected) 0)
			(setq color (canvas-darker color)))
		(.-> this (:ctx_set_color color)
			(:ctx_filled_box 0 0 w h))
		(when (and font text (if (eql (def? :text_cache this) text)
				(defq texture (font-sym-texture font sym_cache))
				(defq texture (font-sym-texture font (set this :text_cache text :sym_cache (sym text))))))
			(bind '(tid tw th) (texture-metrics texture))
			(. this :ctx_blit tid (get :ink_color this) offset (>>> (- h th) 1) tw th))
		this)

	(defmethod :constraint ()
		; (. text :constraint) -> (width height)
		(defq font (get :font this) text (get :text this)
			mw (def? :min_width this) mh (def? :min_height this)
			w 0 h 0)
		(setd mw 0 mh 0)
		(and font text (bind '(w h) (font-glyph-bounds font text)))
		(list (max w mw) (max h mh)))
	)
