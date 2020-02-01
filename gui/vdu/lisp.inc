;lisp bindings
(ffi vdu-load "gui/vdu/lisp_load" 0)

(defun-bind vdu-max-size (vdu)
	;(vdu-max-size vdu) -> (w h)
	(list (/ (get-field vdu (const view_w) (const int_size))
			(get-field vdu (const vdu_char_width) (const (neg short_size))))
		(/ (get-field vdu (const view_h) (const int_size))
			(get-field vdu (const vdu_char_height) (const (neg short_size))))))
