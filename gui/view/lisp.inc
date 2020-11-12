;lisp bindings
(ffi view-add-back "gui/view/lisp_add_back" 0)
(ffi view-add-dirty "gui/view/lisp_add_dirty" 0)
(ffi view-add-front "gui/view/lisp_add" 0)
(ffi view-clr-opaque "gui/view/lisp_clr_opaque" 0)
(ffi view-add-opaque "gui/view/lisp_add_opaque" 0)
(ffi view-change "gui/view/lisp_change" 0)
(ffi view-change-dirty "gui/view/lisp_change_dirty" 0)
(ffi view-dirty "gui/view/lisp_dirty" 0)
(ffi view-find-id "gui/view/lisp_find_id" 0)
(ffi view-sub "gui/view/lisp_sub" 0)
(ffi view-hide "gui/view/lisp_hide" 0)
(ffi view-sub-opaque "gui/view/lisp_sub_opaque" 0)
(ffi view-set-flags "gui/view/lisp_set_flags" 0)
(ffi view-children "gui/view/lisp_children" 0)
(ffi view-to-front "gui/view/lisp_to_front" 0)
(ffi view-to-back "gui/view/lisp_to_back" 0)
(ffi view-pref-size "gui/view/lisp_pref_size" 0)

(defun view-get-size (view)
	(list (get-field view (const view_w) (const int_size))
		(get-field view (const view_h) (const int_size))))

(defun view-get-bounds (view)
	; (view-get-bounds view) -> (x y w h)
	(list (get-field view (const view_x) (const int_size))
		(get-field view (const view_y) (const int_size))
		(get-field view (const view_w) (const int_size))
		(get-field view (const view_h) (const int_size))))

(defun view-set-bounds (view x y w h)
	; (view-set-bounds view x y w h) -> view
	(set-field (set-field (set-field (set-field view
		(const view_x) (const int_size) x)
		(const view_y) (const int_size) y)
		(const view_w) (const int_size) w)
		(const view_h) (const int_size) h))

(defun view-fit (x y w h)
	; (view-fit x y w h) -> (x y w h)
	(bind '(_ _ sw sh) (gui-info))
	(setq x (max 0 (min x (- sw w))) y (max 0 (min y (- sh h))))
	(list x y (min w sw) (min h sh)))

(defun view-locate (w h &optional p)
	; (view-locate w h [flag]) -> (x y w h)
	(setd p :center)
	(bind '(mx my sw sh) (gui-info))
	(defq x (- mx (/ w 2)) y (- my (/ h 2)))
	(case p
		(:top (setq y my))
		(:left (setq x mx))
		(:bottom (setq y (- my h -1)))
		(:right (setq x (- mx w -1))))
	(view-fit x y w h))

;view flags
(defq +view_flag_solid+ 1 +view_flag_opaque+ 2 +view_flag_dirty_all+ 4 +view_flag_at_back+ 8)

(class View ()
	; (View) -> view
	; overide the default 'this' env with a flow component
	(defq this ((ffi _ "gui/view/lisp_create" 0)))

	(method :add_child (this child)
		; (. view :add_child child) -> view
		(view-add-back this child))

	(method :get_pos (this)
		; (. view :get_pos) -> (x y)
		(list (get-field this (const view_x) (const int_size))
			(get-field this (const view_y) (const int_size))))

	(method :get_size (this)
		; (. view :get_size) -> (width height)
		(list (get-field this (const view_w) (const int_size))
			(get-field this (const view_h) (const int_size))))

	(method :set_size (this width height)
		; (. view :set_size width height) -> view
		(set-field (set-field this
			(const view_w) (const int_size) width)
			(const view_h) (const int_size) height))

	(method :get_bounds (this)
		; (. view :get_bounds) -> (x y width height)
		(list (get-field this (const view_x) (const int_size))
			(get-field this (const view_y) (const int_size))
			(get-field this (const view_w) (const int_size))
			(get-field this (const view_h) (const int_size))))

	(method :set_bounds (this x y width height)
		; (. view :set_size x y width height) -> view
		(set-field (set-field (set-field (set-field this
			(const view_x) (const int_size) x)
			(const view_y) (const int_size) y)
			(const view_w) (const int_size) width)
			(const view_h) (const int_size) height))

	;; (method :pref_size (this)
	;; 	; (. view :pref_size) -> (width height)
	;; 	(defq mw (get :min_width this) mw (get :min_height this))
	;; 	(setd mw 0 mh 0)
	;; 	(list mw mh))

	(method :layout (this)
		; (. view :layout) -> view
		(if (= (>> (get :color this) 24) 0xff)
			(view-set-flags this +view_flag_opaque+ +view_flag_opaque+))
		this)
	)
