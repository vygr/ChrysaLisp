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

;the most of a text that is drawn, in bytes. A line is one texture, and
;none is wanted wider than any screen
(defq +text_most 512)

(defun text-most (text)
	; (text-most text) -> text
	;the text, or its first +text_most bytes, cut between characters
	(when (> (length text) +text_most)
		(defq at +text_most)
		(while (= (logand (code text 1 at) 0xc0) 0x80) (-- at))
		(setq text (slice text 0 at)))
	text)

(defclass Text () (View)
	; (Text) -> text
	(def this :paper 0 :state 0
		:sym_cache :nil :text_cache :nil :offset 0)

	(defmethod :draw ()
		; (. text :draw) -> text
		(raise :font :text :offset :sym_cache :state :paper)
		(bind '(w h) (. this :get_size))
		(if (/= (logand state +text_state_highlight) 0)
			(setq color *env_highlight_col*))
		(if (/= (logand state +text_state_selected) 0)
			(setq color (canvas-darker color)))
		(.-> this (:ctx_set_color color) (:ctx_filled_box 0 0 w h))
		(when (and font text (if (eql (def? :text_cache this) text)
				(defq texture (font-sym-texture font sym_cache))
				(defq texture (font-sym-texture font (set this :text_cache text :sym_cache (sym (text-most text)))))))
			(bind '(tid tw th) (texture-metrics texture))
			(. this :ctx_blit tid (get :ink this) offset (>>> (- h th) 1) tw th))
		this)

	(defmethod :constraint ()
		; (. text :constraint) -> (width height)
		(defq font (get :font this) text (get :text this) w 0 h 0)
		(and font text (bind '(w h) (font-glyph-bounds font (text-most text))))
		(list (max w (ifn (def? :min_width this) 0))
			(max h (ifn (def? :min_height this) 0))))
	)
