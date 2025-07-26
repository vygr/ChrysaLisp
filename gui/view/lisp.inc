;lisp bindings
(import "service/gui/lisp.inc")
(import "sys/list/lisp.inc")
(import "class/hmap/lisp.inc")

(structure +view +hmap_size
	(offset start)
	(struct node +ln_node_size)
	(struct list +lh_list_size)
	(netid owner_id)
	(ptr dirty_region opaque_region ctx_node)
	(long id)
	(uint flags)
	(int ctx_x ctx_y x y w h cw ch))

(bits +view_flag 0
	(bit solid opaque dirty_all hidden at_back at_front subtree))

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

(defclass View () :nil
	; (View) -> view
	; override the default 'this' env with a View component
	(defq super this this ((const (ffi "gui/view/lisp_create"))))
	(each (lambda ((key val)) (def this key val)) (tolist super))

	;;;;
	; id
	;;;;

	(defmethod :get_id ()
		; (. view :get_id) -> id
		(getf this +view_id 0))

	(deffimethod :find_id "gui/view/lisp_find_id")
		; (. view :find_id target_id) -> :nil | target_view

	;;;;;;;;;;;
	; ownership
	;;;;;;;;;;;

	(defmethod :find_owner ()
		; (. view :find_owner) -> :nil | netid
		(defq root this)
		(while (and root (eql (defq id (getf root +view_owner_id 0))
					(const (pad "" +net_id_size (ascii-char 0)))))
			(setq root (penv root)))
		(if root id))

	(defmethod :set_owner (id)
		; (. view :set_owner netid) -> view
		(setf this +view_owner_id id 0)
		this)

	;;;;;;;
	; flags
	;;;;;;;

	(deffimethod :set_flags "gui/view/lisp_set_flags")
		; (. view :set_flags value mask) -> view

	(defmethod :get_flags ()
		; (. view :get_flags) -> flags
		(getf this +view_flags 0))

	;;;;;;;;;;;;;;;;;;;
	; position and size
	;;;;;;;;;;;;;;;;;;;

	(defmethod :set_pos (x y)
		; (. view :set_pos x y) -> view
		(setf (setf this +view_x x 0) +view_y y 0)
		this)

	(defmethod :get_pos ()
		; (. view :get_pos) -> (x y)
		(list (getf this +view_x 0) (getf this +view_y 0)))

	(defmethod :get_relative (child)
		; (. view :get_relative child) -> (x y w h)
		(bind '(x y w h) (. child :get_bounds))
		(until (eql (defq p (penv child)) this)
			(bind '(x1 y1) (. p :get_pos))
			(setq x (+ x x1) y (+ y y1) child p))
		(list x y w h))

	(defmethod :set_size (width height)
		; (. view :set_size width height) -> view
		(setf (setf this +view_w width 0) +view_h height 0))

	(defmethod :get_size ()
		; (. view :get_size) -> (width height)
		(list (getf this +view_w 0) (getf this +view_h 0)))

	(defmethod :set_bounds (x y width height)
		; (. view :set_size x y width height) -> view
		(setf (setf (setf (setf this
			+view_x x 0) +view_y y 0) +view_w width 0) +view_h height 0))

	(defmethod :get_bounds ()
		; (. view :get_bounds) -> (x y width height)
		(list (getf this +view_x 0) (getf this +view_y 0)
			(getf this +view_w 0) (getf this +view_h 0)))

	(deffimethod :hit_tree "gui/view/lisp_hit_tree")
		; (. view :hit_tree x y) -> (hit_view | :nil rx ry)

	;;;;;;;;;;;;;;
	; dirty region
	;;;;;;;;;;;;;;

	(deffimethod :add_dirty "gui/view/lisp_add_dirty")
		; (. view :add_dirty x y width height) -> view

	(deffimethod :trans_dirty "gui/view/lisp_trans_dirty")
		; (. view :trans_dirty rx ry) -> view

	(defmethod :dirty ()
		; (. view :dirty) -> view
		(bind '(w h) (. this :get_size))
		(. this :add_dirty 0 0 w h))

	(defmethod :dirty_all ()
		; (. view :dirty_all) -> view
		(. this :set_flags +view_flag_dirty_all +view_flag_dirty_all))

	;;;;;;;;;;;;;;;
	; opaque region
	;;;;;;;;;;;;;;;

	(deffimethod :add_opaque "gui/view/lisp_add_opaque")
		; (. view :add_opaque x y width height) -> view

	(deffimethod :sub_opaque "gui/view/lisp_sub_opaque")
		; (. view :sub_opaque x y width height) -> view

	(deffimethod :clr_opaque "gui/view/lisp_clr_opaque")
		; (. view :clr_opaque) -> view

	;;;;;;;;;;;;;;;
	; tree building
	;;;;;;;;;;;;;;;

	(deffimethod :sub "gui/view/lisp_sub")
		; (. view :lisp_sub) -> view

	(deffimethod :add_back "gui/view/lisp_add_back")
		; (. view :add_back child) -> view

	(deffimethod :add_front "gui/view/lisp_add")
		; (. view :add_front child) -> view

	(defmethod :add_child (child)
		; (. view :add_child child) -> view
		(. this :add_back child))

	(deffimethod :to_front "gui/view/lisp_to_front")
		; (. view :to_front) -> view

	(deffimethod :to_back "gui/view/lisp_to_back")
		; (. view :to_back) -> view

	(deffimethod :hide "gui/view/lisp_hide")
		; (. view :hide) -> view

	;;;;;;;;;;;;;
	; enumeration
	;;;;;;;;;;;;;

	(deffimethod :children "gui/view/lisp_children")
		; (. view :children) -> (child0 child1 ...)

	(deffimethod :flatten "gui/view/lisp_flatten")
		; (. view :flatten) -> (child0 child1 ...)

	;;;;;;;;;;;;;;;;;
	; drawing context
	;;;;;;;;;;;;;;;;;

	(deffimethod :ctx_set_color "gui/ctx/lisp_set_color")
		; (. view :ctx_set_color col) -> view

	(deffimethod :ctx_box "gui/ctx/lisp_box")
		; (. view :ctx_box x y width height) -> view

	(deffimethod :ctx_filled_box "gui/ctx/lisp_filled_box")
		; (. view :ctx_filled_box x y width height) -> view

	(deffimethod :ctx_blit "gui/ctx/lisp_blit")
		; (. view :ctx_blit tid col x y width height) -> view

	(defmethod :ctx_panel (col flags depth x y width height)
		; (. view :ctx_panel col flags depth x y width height) -> view
		(defq abs_depth (abs depth))
		(when (/= flags 0)
			;fill middle
			(.-> this
				(:ctx_set_color col)
				(:ctx_filled_box (+ x abs_depth) (+ y abs_depth)
					(- width abs_depth abs_depth) (- height abs_depth abs_depth))))
		(cond
			((< depth 0)
				;suncken
				(.-> this
					(:ctx_set_color (canvas-darker col))
					(:ctx_filled_box x y (- width abs_depth) abs_depth)
					(:ctx_filled_box x (+ y abs_depth) abs_depth (- height abs_depth abs_depth))
					(:ctx_set_color (canvas-brighter col))
					(:ctx_filled_box x (+ y height depth) width abs_depth)
					(:ctx_filled_box (+ x width depth) y abs_depth (- height abs_depth))))
			((> depth 0)
				;raised
				(.-> this
					(:ctx_set_color (canvas-brighter col))
					(:ctx_filled_box x y width abs_depth)
					(:ctx_filled_box x (+ y abs_depth) abs_depth (- height abs_depth))
					(:ctx_set_color (canvas-darker col))
					(:ctx_filled_box (+ x abs_depth) (- (+ y height) abs_depth) (- width abs_depth) abs_depth)
					(:ctx_filled_box (- (+ x width) abs_depth) (+ y abs_depth) abs_depth (- height abs_depth abs_depth)))))
		this)

	;;;;;;;;;;;;;;
	; size metrics
	;;;;;;;;;;;;;;

	(defmethod :constraints ()
		; (. view :constraints) -> view
		(each (# (bind '(w h) (. %0 :constraint)) (. %0 :set_constraint w h))
			(. this :flatten)) this)

	(defmethod :constraint ()
		; (. view :constraint) -> (width height)
		(list (ifn (def? :min_width this) 0) (ifn (def? :min_height this) 0)))

	(defmethod :get_constraint ()
		; (. view :get_constraint) -> (width height)
		(list (getf this +view_cw 0) (getf this +view_ch 0)))

	(defmethod :set_constraint (width height)
		; (. view :set_constraint width height) -> view
		(setf (setf this +view_cw width 0) +view_ch height 0)
		this)

	(defmethod :pref_size ()
		; (. view :pref_size) -> (width height)
		(.-> this :constraints :get_constraint))

	;;;;;;;;;
	; layouts
	;;;;;;;;;

	(defmethod :constrain (&optional flag)
		; (. view :constrain [flag]) -> view
		(defq nodes (. this :flatten))
		(each (# (bind '(w h) (. %0 :constraint)) (. %0 :set_constraint w h)) nodes)
		(cond
			(flag
				;forced layout
				(reach (# (. %0 :layout)) nodes))
			(:t ;layout if size change
				(defq sizes (map (# (. %0 :get_size)) nodes))
				(. this :layout)
				(reach (lambda (node (ow oh))
					(bind '(w h) (. node :get_size))
					(if (or (/= w ow) (/= h oh)) (. node :layout))) nodes sizes)))
		this)

	(defmethod :layout ()
		; (. view :layout) -> view
		(if (= (>> (get :color this) 24) 0xff)
			(. this :set_flags +view_flag_opaque +view_flag_opaque)
			this))

	(defmethod :change (x y width height &optional flag)
		; (. view :change x y width height [flag]) -> view
		(bind '(old_width old_height) (. this :get_size))
		(. this :set_bounds x y width height)
		(if (or flag (/= width old_width) (/= height old_height))
			(. this :constrain flag)
			this))

	(defmethod :change_dirty (x y width height)
		; (. view :change_dirty x y width height) -> view
		(bind '(old_x old_y) (. this :get_pos))
		(.-> this :dirty (:change x y width height)
			(:trans_dirty (- old_x x) (- old_y y)) :dirty_all))

	;;;;;;;;;;;;;;;;;
	; signal emittion
	;;;;;;;;;;;;;;;;;

	(defmethod :connect (id)
		; (. view :connect id) -> view
		(unless (defq targets (def? :targets this))
			(def this :targets (setq targets (array))))
		(push targets id)
		this)

	(defmethod :emit ()
		; (. view :emit) -> view
		(when (and (defq targets (def? :targets this))
				(defq mbox (. this :find_owner)))
			(defq source_id (. this :get_id))
			(each (lambda (id)
				(mail-send mbox (setf-> (str-alloc +ev_msg_action_size)
					(+ev_msg_type +ev_type_action)
					(+ev_msg_target_id id)
					(+ev_msg_action_source_id source_id)))) targets))
		this)
	)
