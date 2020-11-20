;;;;;;;;;;;;
; vdu widget
;;;;;;;;;;;;

(defclass Vdu ()
	; (Vdu) -> vdu
	; overide the default 'this' env with a vdu component
	(defq this ((ffi _ "gui/vdu/lisp_create" 0)))

	(defmethod :max_size (this)
		; (. vdu :max_size) -> (width height)
		(list (/ (get-field this (const view_w) (const int_size))
				(get-field this (const vdu_char_width) (const (neg short_size))))
			(/ (get-field this (const view_h) (const int_size))
				(get-field this (const vdu_char_height) (const (neg short_size))))))

	(defmethod :char_size (this)
		; (. vdu :char_size) -> (width height)
		(list (get-field this (const vdu_char_width) (const (neg short_size)))
			(get-field this (const vdu_char_height) (const (neg short_size)))))

	(defmethod :load (this lines offset_x offset_y cursor_x cursor_y)
		; (. vdu :load lines offset_x offset_y cursor_x cursor_y) -> vdu
		((ffi _ "gui/vdu/lisp_load" 0) this lines offset_x offset_y cursor_x cursor_y))
	)
