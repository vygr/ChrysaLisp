;;;;;;;;;;;;
; vdu widget
;;;;;;;;;;;;

(import "././view/lisp.inc")

(structure +vdu +view_size
	(ptr font textures)
	(plong chars)
	(uint width height)
	(ushort char_width char_height))

(defclass Vdu () (View)
	; (Vdu) -> vdu
	; override the default 'this' env with a Vdu component
	(defq super this this ((ffi "gui/vdu/lisp_create")))
	(each (lambda ((key val)) (def this key val)) (tolist super))

	(defmethod :char_size ()
		; (. vdu :char_size) -> (width height)
		((const (ffi "gui/vdu/lisp_configure")) this)
		(list (getf this +vdu_char_width 0) (getf this +vdu_char_height 0)))

	(defmethod :max_size ()
		; (. vdu :max_size) -> (width height)
		(bind '(cw ch) (. this :char_size))
		(list (/ (getf this +view_w 0) cw) (/ (getf this +view_h 0) ch)))

	(defmethod :vdu_size ()
		; (. vdu :vdu_size) -> (width height)
		(list (get :vdu_width this) (get :vdu_height this)))

	(defmethod :load (lines offset_x offset_y cursor_x cursor_y)
		; (. vdu :load lines offset_x offset_y cursor_x cursor_y) -> vdu
		(. ((const (ffi "gui/vdu/lisp_load")) this lines offset_x offset_y cursor_x cursor_y) :dirty))

	(defmethod :constraint ()
		; (. vdu :constraint) -> (width height)
		(bind '(cw ch) (. this :char_size))
		(list (* (ifn (def? :min_width this) (get :vdu_width this)) cw)
			(* (ifn (def? :min_height this) (get :vdu_height this)) ch)))

	(defmethod :layout ()
		; (. vdu :layout) -> vdu
		((const (ffi "gui/vdu/lisp_configure")) this))
	)
