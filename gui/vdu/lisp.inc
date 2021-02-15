;;;;;;;;;;;;
; vdu widget
;;;;;;;;;;;;

(defclass Vdu () (View)
	; (Vdu) -> vdu
	; overide the default 'this' env with a Vdu component
	(defq super this this ((ffi _ "gui/vdu/lisp_create" 0)))
	(each (lambda ((key val)) (def this key val)) (tolist super))

	(defmethod :max_size (this)
		; (. vdu :max_size) -> (width height)
		(list (/ (get-field this (const view_w) +int_size+)
				(get-field this (const vdu_char_width) (const (neg +short_size+))))
			(/ (get-field this (const view_h) +int_size+)
				(get-field this (const vdu_char_height) (const (neg +short_size+))))))

	(defmethod :char_size (this)
		; (. vdu :char_size) -> (width height)
		(list (get-field this (const vdu_char_width) (const (neg +short_size+)))
			(get-field this (const vdu_char_height) (const (neg +short_size+)))))

	(defmethod :load (this lines offset_x offset_y cursor_x cursor_y)
		; (. vdu :load lines offset_x offset_y cursor_x cursor_y) -> vdu
		(. ((const (ffi _ "gui/vdu/lisp_load" 0)) this lines offset_x offset_y cursor_x cursor_y) :dirty))

	(defmethod :pref_size (this)
		; (. vdu :pref_size) -> (width height)
		((const (ffi _ "gui/vdu/lisp_configure" 0)) this)
		(defq vdu_width (get :vdu_width this) vdu_height (get :vdu_height this)
			mw (get :min_width this) mh (get :min_height this)
			cw (get-field this (const vdu_char_width) (const (neg +short_size+)))
			ch (get-field this (const vdu_char_height) (const (neg +short_size+))))
		(list (* (if mw mw vdu_width) cw) (* (if mh mh vdu_height) ch)))
	)
