;lisp bindings
(ffi view-add-back "gui/view/lisp_add_back" 0)
(ffi view-add-child "gui/view/lisp_add_child" 0)
(ffi view-add-dirty "gui/view/lisp_add_dirty" 0)
(ffi view-add-front "gui/view/lisp_add" 0)
(ffi view-add-opaque "gui/view/lisp_add_opaque" 0)
(ffi view-change "gui/view/lisp_change" 0)
(ffi view-change-dirty "gui/view/lisp_change_dirty" 0)
(ffi view-dirty "gui/view/lisp_dirty" 0)
(ffi view-event "gui/view/lisp_event" 0)
(ffi view-find-id "gui/view/lisp_find_id" 0)
(ffi view-layout "gui/view/lisp_layout" 0)
(ffi view-pref-size "gui/view/lisp_pref_size" 0)
(ffi view-sub "gui/view/lisp_sub" 0)
(ffi view-hide "gui/view/lisp_hide" 0)
(ffi view-sub-opaque "gui/view/lisp_sub_opaque" 0)
(ffi view-set-flags "gui/view/lisp_set_flags" 0)

(defun-bind view-get-pos (view)
	;(view-get-pos view) -> (x y)
	(list (get-field view (const view_x) (const int_size))
		(get-field view (const view_y) (const int_size))))

(defun-bind view-set-pos (view x y)
	;(view-set-pos view x y) -> view
	(set-field (set-field view
		(const view_x) (const int_size) x)
		(const view_y) (const int_size) y))

(defun-bind view-get-size (view)
	;(view-get-size view) -> (w h)
	(list (get-field view (const view_w) (const int_size))
		(get-field view (const view_h) (const int_size))))

(defun-bind view-set-size (view w h)
	;(view-set-size view w h) -> view
	(set-field (set-field view
		(const view_w) (const int_size) w)
		(const view_h) (const int_size) h))

(defun-bind view-get-bounds (view)
	;(view-get-bounds view) -> (x y w h)
	(list (get-field view (const view_x) (const int_size))
		(get-field view (const view_y) (const int_size))
		(get-field view (const view_w) (const int_size))
		(get-field view (const view_h) (const int_size))))

(defun-bind view-set-bounds (view x y w h)
	;(view-set-bounds view x y w h) -> view
	(set-field (set-field (set-field (set-field view
		(const view_x) (const int_size) x)
		(const view_y) (const int_size) y)
		(const view_w) (const int_size) w)
		(const view_h) (const int_size) h))
