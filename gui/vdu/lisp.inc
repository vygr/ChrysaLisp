;;;;;;;;;;;;
; vdu widget
;;;;;;;;;;;;

(ffi create-vdu "gui/vdu/lisp_create" 0)
(ffi vdu-load "gui/vdu/lisp_load" 0)

(class Vdu ()
	; (Vdu) -> vdu
	; overide the default 'this' env with a vdu component
	(defq this (create-vdu))

	(method :max_size (this)
		; (. vdu :max_size) -> (width height)
		(list (/ (get-field this (const view_w) (const int_size))
				(get-field this (const vdu_char_width) (const (neg short_size))))
			(/ (get-field this (const view_h) (const int_size))
				(get-field this (const vdu_char_height) (const (neg short_size))))))

	(method :char_size (this)
		; (. vdu :char_size) -> (width height)
		(list (get-field this (const vdu_char_width) (const (neg short_size)))
			(get-field this (const vdu_char_height) (const (neg short_size)))))
	)
