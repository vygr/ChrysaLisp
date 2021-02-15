;lisp bindings

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
(defq +view_flag_solid+ 1 +view_flag_opaque+ 2 +view_flag_dirty_all+ 4
	+view_flag_at_back+ 8 +view_flag_hiden+ 16)

(defclass View () nil
	; (View) -> view
	; overide the default 'this' env with a View component
	(defq super this this ((const (ffi _ "gui/view/lisp_create" 0))))
	(each (lambda ((key val)) (def this key val)) (tolist super))

	(deffimethod :find_id "gui/view/lisp_find_id")
		; (. view :find_id target_id) -> nil | target_view

	(deffimethod :set_flags "gui/view/lisp_set_flags")
		; (. view :set_flags value mask) -> view

	(deffimethod :add_dirty "gui/view/lisp_add_dirty")
		; (. view :add_dirty x y width height) -> view

	(deffimethod :trans_dirty "gui/view/lisp_trans_dirty")
		; (. view :trans_dirty rx ry) -> view

	(deffimethod :add_opaque "gui/view/lisp_add_opaque")
		; (. view :add_opaque x y width height) -> view

	(deffimethod :sub_opaque "gui/view/lisp_sub_opaque")
		; (. view :sub_opaque x y width height) -> view

	(deffimethod :clr_opaque "gui/view/lisp_clr_opaque")
		; (. view :clr_opaque) -> view

	(deffimethod :children "gui/view/lisp_children")
		; (. view :children) -> (child0 child1 ...)

	(deffimethod :sub "gui/view/lisp_sub")
		; (. view :lisp_sub) -> view

	(deffimethod :add_back "gui/view/lisp_add_back")
		; (. view :add_back child) -> view

	(deffimethod :add_front "gui/view/lisp_add")
		; (. view :add_front child) -> view

	(deffimethod :to_front "gui/view/lisp_to_front")
		; (. view :to_front) -> view

	(deffimethod :to_back "gui/view/lisp_to_back")
		; (. view :to_back) -> view

	(deffimethod :hide "gui/view/lisp_hide")
		; (. view :hide) -> view

	(deffimethod :emit "gui/view/lisp_emit")
		; (. view :emit) -> view

	(deffimethod :ctx_set_color "gui/ctx/lisp_set_color")
		; (. view :ctx_set_color col) -> view

	(deffimethod :ctx_box "gui/ctx/lisp_box")
		; (. view :ctx_box x y width height) -> view

	(deffimethod :ctx_filled_box "gui/ctx/lisp_filled_box")
		; (. view :ctx_filled_box x y width height) -> view

	(deffimethod :ctx_blit "gui/ctx/lisp_blit")
		; (. view :ctx_filled_box tid col x y width height) -> view

	(deffimethod :ctx_panel "gui/ctx/lisp_panel")
		; (. view :ctx_panel col flags depth x y width height) -> view

	(defmethod :connect (this id)
		; (. view :connect id) -> view
		(unless (defq targets (get-field this (const view_target_ids) 0))
			(set-field this (const view_target_ids) 0 (setq targets (array))))
		(push targets id)
		this)

	(defmethod :get_id (this)
		; (. view :get_id) -> id
		(get-field this (const view_id) +long_size+))

	(defmethod :dirty (this)
		; (. view :dirty) -> view
		(bind '(w h) (. this :get_size))
		(. this :add_dirty 0 0 w h))

	(defmethod :dirty_all (this)
		; (. view :dirty_all) -> view
		(. this :set_flags +view_flag_dirty_all+ +view_flag_dirty_all+))

	(defmethod :get_flags (this)
		; (. view :get_flags) -> flags
		(get-field this (const view_flags) (const (neg +int_size+))))

	(defmethod :add_child (this child)
		; (. view :add_child child) -> view
		(. this :add_back child))

	(defmethod :get_pos (this)
		; (. view :get_pos) -> (x y)
		(list (get-field this (const view_x) +int_size+)
			(get-field this (const view_y) +int_size+)))

	(defmethod :get_size (this)
		; (. view :get_size) -> (width height)
		(list (get-field this (const view_w) +int_size+)
			(get-field this (const view_h) +int_size+)))

	(defmethod :set_size (this width height)
		; (. view :set_size width height) -> view
		(set-field (set-field this
			(const view_w) +int_size+ width)
			(const view_h) +int_size+ height))

	(defmethod :get_bounds (this)
		; (. view :get_bounds) -> (x y width height)
		(list (get-field this (const view_x) +int_size+)
			(get-field this (const view_y) +int_size+)
			(get-field this (const view_w) +int_size+)
			(get-field this (const view_h) +int_size+)))

	(defmethod :set_bounds (this x y width height)
		; (. view :set_size x y width height) -> view
		(set-field (set-field (set-field (set-field this
			(const view_x) +int_size+ x)
			(const view_y) +int_size+ y)
			(const view_w) +int_size+ width)
			(const view_h) +int_size+ height))

	(defmethod :change (this x y width height)
		; (. view :change x y width height) -> view
		(bind '(old_width old_height) (. this :get_size))
		(. this :set_bounds x y width height)
		(unless (and (= width old_width) (= height old_height))
			(. this :layout))
		this)

	(defmethod :change_dirty (this x y width height)
		; (. view :change_dirty x y width height) -> view
		(bind '(old_x old_y) (. this :get_pos))
		(.-> this :dirty (:change x y width height)
			(:trans_dirty (- old_x x) (- old_y y)) :dirty_all))

	(defmethod :pref_size (this)
		; (. view :pref_size) -> (width height)
		(defq mw (get :min_width this) mh (get :min_height this))
		(setd mw 0 mh 0)
		(list mw mh))

	(defmethod :layout (this)
		; (. view :layout) -> view
		(if (= (>> (get :color this) 24) 0xff)
			(. this :set_flags +view_flag_opaque+ +view_flag_opaque+)
			this))
	)
