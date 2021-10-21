;;;;;;;;;;;;
; vdu widget
;;;;;;;;;;;;

(import "gui/view/lisp.inc")

(structure +vdu +view_size
	(ptr font textures)
	(plong chars)
	(uint width height)
	(ushort char_width char_height))

(defclass Vdu () (View)
	; (Vdu) -> vdu
	; overide the default 'this' env with a Vdu component
	(defq super this this ((ffi _ "gui/vdu/lisp_create" 0)))
	(each (lambda ((key val)) (def this key val)) (tolist super))

	(defmethod :char_size (this)
		; (. vdu :char_size) -> (width height)
		((const (ffi _ "gui/vdu/lisp_configure" 0)) this)
		(list (getf this +vdu_char_width 0) (getf this +vdu_char_height 0)))

	(defmethod :max_size (this)
		; (. vdu :max_size) -> (width height)
		(bind '(cw ch) (. this :char_size))
		(list (/ (getf this +view_w 0) cw) (/ (getf this +view_h 0) ch)))

	(defmethod :vdu_size (this)
		; (. vdu :vdu_size) -> (width height)
		(list (get :vdu_width this) (get :vdu_height this)))

	(defmethod :load (this lines offset_x offset_y cursor_x cursor_y)
		; (. vdu :load lines offset_x offset_y cursor_x cursor_y) -> vdu
		(. ((const (ffi _ "gui/vdu/lisp_load" 0)) this lines offset_x offset_y cursor_x cursor_y) :dirty))

	(defmethod :pref_size (this)
		; (. vdu :pref_size) -> (width height)
		(bind '(cw ch) (. this :char_size))
		(defq vdu_width (get :vdu_width this) vdu_height (get :vdu_height this)
			mw (def? :min_width this) mh (def? :min_height this))
		(list (* (if mw mw vdu_width) cw) (* (if mh mh vdu_height) ch)))

	(defmethod :layout (this)
		; (. vdu :layout) -> this
		((const (ffi _ "gui/vdu/lisp_configure" 0)) this)
		this)
	)
