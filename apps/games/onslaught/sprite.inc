;;;;;;;;;;;;;;;;;;;
; base sprite class
;;;;;;;;;;;;;;;;;;;

(defclass Sprite (canvas_l canvas_r width height tag &optional death) (View)
	; (Sprite canvas_l canvas_r width height [death]) -> sprite
	(def this :color 0
		:sp_x 0 :sp_y 0 :sp_w 0 :sp_h 0
		:sp_canvas_l canvas_l :sp_canvas_r canvas_r :sp_canvas_tag tag
		:sp_frame 0 :sp_dir -1 :sp_components (list) :sp_death (ifn death :nil))
	(.-> this (:sp_set_pos 0 0) (:sp_set_size width height))

	(defmethod :sp_set_pos (x y)
		; (. sprite :sp_set_pos x y) -> sprite
		(def this :sp_x x :sp_y y)
		(. this :set_pos (* *zoom* x) (* *zoom* y)))

	(defmethod :sp_get_pos ()
		; (. sprite :sp_get_pos) -> (x y)
		(list (get :sp_x this) (get :sp_y this)))

	(defmethod :sp_set_size (w h)
		; (. sprite :sp_set_size w h) -> sprite
		(def this :sp_w w :sp_h h)
		(. this :set_size (* *zoom* w) (* *zoom* h)))

	(defmethod :sp_get_size ()
		; (. sprite :sp_get_size) -> (w h)
		(list (get :sp_w this) (get :sp_h this)))

	(defmethod :sp_set_bounds (x y w h)
		; (. sprite :sp_set_bounds x y w h) -> sprite
		(def this :sp_x x :sp_y y :sp_w w :sp_h h)
		(. this :set_bounds (* *zoom* x) (* *zoom* y) (* *zoom* w) (* *zoom* h)))

	(defmethod :sp_get_bounds ()
		; (. sprite :sp_get_bounds) -> (x y w h)
		(list (get :sp_x this) (get :sp_y this) (get :sp_w this) (get :sp_h this)))

	(defmethod :get_canvas ()
		; (. sprite :get_canvas) -> canvas
		(raise :sp_canvas_l :sp_canvas_r)
		(if (and sp_canvas_r (>= (or (get :sp_dir this) -1) 0)) sp_canvas_r sp_canvas_l))

	(defmethod :kill ()
		; (. sprite :kill) -> sprite
		(unless (= (get :sp_frame this) -1)
			(def this :sp_frame -1)
			(when (defq death (get :sp_death this))
				(def this :sp_death :nil)
				(death this))
			(. this :sub))
		this)

	(defmethod :set_frame (f)
		; (. sprite :set_frame f) -> sprite
		(when (/= f (get :sp_frame this))
			(def this :sp_frame f)
			(when (= f -1)
				(. this :kill)))
		this)

	(defmethod :add_component (update &optional init)
		; (. sprite :add_component update [init]) -> sprite
		(when init (init this))
		(push (get :sp_components this) update)
		this)

	(defmethod :update ()
		; (. sprite :update) -> sprite
		(if (= (get :sp_frame this) -1)
			(. this :kill)
			(progn
				(each (lambda (fn)
					(unless (= (get :sp_frame this) -1)
						(fn this)))
					(get :sp_components this))
				(when (= (get :sp_frame this) -1)
					(. this :kill))))
		this)

	(defmethod :draw ()
		; (. sprite :draw) -> sprite
		(when (/= (defq f (get :sp_frame this)) -1)
			(when (defq c (. this :get_canvas))
				(when (defq texture (getf c +canvas_texture 0))
					(bind '(tid tw th) (texture-metrics texture))
					(bind '(w h) (. this :get_size))
					(defq raw_y (* f h) col (/ raw_y th)
						sy (% raw_y th) sx (* col w)
						dir (or (get :sp_dir this) 1))
					(when (and (>= dir 0) (get :sp_canvas_r this))
						(setq sx (- tw (+ sx w))))
					(. this :ctx_blit tid +argb_white 0 0 w h sx sy))))
		this)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; standard component suite
;;;;;;;;;;;;;;;;;;;;;;;;;;

; movement by vectors

(defun mv-init (this)
	(def this :mv_vx 0 :mv_vy 0 :mv_ax 0 :mv_ay 0 :mv_max_vx 100 :mv_max_vy 100)
	this)

(defun mv-update (this)
	(bind '(x y) (. this :sp_get_pos))
	(defq ax (get :mv_ax this) ay (get :mv_ay this)
		max_vx (get :mv_max_vx this) max_vy (get :mv_max_vy this)
		vx (max (neg max_vx) (min max_vx (+ (get :mv_vx this) ax)))
		vy (max (neg max_vy) (min max_vy (+ (get :mv_vy this) ay))))
	(set this :mv_vx vx :mv_vy vy)
	(. this :sp_set_pos (+ x vx) (+ y vy))
	this)

; movement by vector table

(defun mt-init (this)
	(def this :mt_table :nil :mt_speed 1 :mt_count 0 :mt_index 0)
	this)

(defun mt-update (this)
	(when (defq table (get :mt_table this))
		(defq speed (get :mt_speed this))
		(when (> speed 0)
			(defq count (inc (get :mt_count this)))
			(if (>= count speed)
				(progn
					(set this :mt_count 0)
					(defq idx (get :mt_index this))
					(bind '(dx dy) (elem-get table idx))
					(bind '(x y) (. this :sp_get_pos))
					(. this :sp_set_pos (+ x dx) (+ y dy))
					(defq next_idx (inc idx))
					(if (>= next_idx (length table))
						(setq next_idx 0))
					(set this :mt_index next_idx))
				(set this :mt_count count))))
	this)

; movement by linear interpolation

(defun ml-init (this)
	(def this :ml_target_x 0 :ml_target_y 0 :ml_speed 0 :ml_count -1
		:ml_dx 0 :ml_dy 0 :ml_d1x 0 :ml_d2x 0
		:ml_d1y 0 :ml_d2y 0 :ml_slope 0)
	this)

(defun ml-update (this)
	(defq speed (get :ml_speed this))
	(when (> speed 0)
		(defq slope (get :ml_slope this) count (get :ml_count this)
			dx (get :ml_dx this) dy (get :ml_dy this)
			d1x (get :ml_d1x this) d2x (get :ml_d2x this)
			d1y (get :ml_d1y this) d2y (get :ml_d2y this))
		(bind '(x y) (. this :sp_get_pos))
		(while (and (> speed 0) (> count 0))
			(-- count)
			(setq slope (- slope dy))
			(if (< slope 0)
				(setq slope (+ slope dx) x (+ x d1x) y (+ y d1y))
				(setq x (+ x d2x) y (+ y d2y)))
			(-- speed))
		(when (<= count 0)
			(setq count -1 x (get :ml_target_x this) y (get :ml_target_y this)))
		(set this :ml_slope slope :ml_count count)
		(. this :sp_set_pos x y))
	this)

(defun init-ml (this x y x1 y1 &optional speed)
	; (init-ml sprite x y x1 y1 [speed]) -> sprite
	(when speed (set this :ml_speed speed))
	(. this :sp_set_pos x y)
	(defq dx (- x1 x) dy (- y1 y)
		d1x (cond ((< dx 0) -1) ((> dx 0) 1) (:t 0)) d2x d1x
		d1y (cond ((< dy 0) -1) ((> dy 0) 1) (:t 0)) d2y 0
		abs_dx (abs dx) abs_dy (abs dy) tmpi 0)
	(when (< abs_dx abs_dy)
		(setq d2y d1y d2x 0 tmpi abs_dx abs_dx abs_dy abs_dy tmpi))
	(set this :ml_target_x x1 :ml_target_y y1
		:ml_dx abs_dx :ml_dy abs_dy :ml_d1x d1x :ml_d2x d2x :ml_d1y d1y :ml_d2y d2y
		:ml_count abs_dx :ml_slope (>> abs_dx 1))
	this)

; animation by table

(defun at-init (this)
	(def this :at_table :nil :at_speed 1 :at_count 0 :at_index 0)
	this)

(defun at-update (this)
	(when (defq table (get :at_table this))
		(defq speed (get :at_speed this))
		(when (> speed 0)
			(defq count (inc (get :at_count this)))
			(if (>= count speed)
				(progn
					(set this :at_count 0)
					(defq idx (get :at_index this) f (elem-get table idx))
					(if (= f -1)
						(. this :kill)
						(progn
							(. this :set_frame f)
							(defq next_idx (inc idx))
							(if (>= next_idx (length table))
								(setq next_idx 0))
							(set this :at_index next_idx))))
				(set this :at_count count))))
	this)

; offscreen killing

(defun offscreen-update (this)
	(bind '(x y w h) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< x 0) (< y 0) (>= (+ x w) ww) (>= (+ y h) wh))
		(. this :kill))
	this)
