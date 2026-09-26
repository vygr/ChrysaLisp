;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught standard component suite
; small bag of standard reusable components operating on sprite properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. MV (movement vectors: x/y velocity and acceleration)
(defun mv-init (this)
	(def this :mv_vx 0 :mv_vy 0 :mv_ax 0 :mv_ay 0 :mv_max_vx 100 :mv_max_vy 100)
	this)

(defun mv-update (this)
	(bind '(x y) (. this :sp_get_pos))
	(defq
		ax (get :mv_ax this)
		ay (get :mv_ay this)
		max_vx (get :mv_max_vx this)
		max_vy (get :mv_max_vy this)
		vx (max (neg max_vx) (min max_vx (+ (get :mv_vx this) ax)))
		vy (max (neg max_vy) (min max_vy (+ (get :mv_vy this) ay))))
	(set this :mv_vx vx :mv_vy vy)
	(. this :sp_set_pos (+ x vx) (+ y vy))
	this)

; 2. ML (movement line: straight line bresenham interpolation / fly-to)
(defun ml-init (this)
	(def this :ml_target_x 0 :ml_target_y 0 :ml_speed 0 :ml_count -1
		:ml_dx 0 :ml_dy 0 :ml_d1x 0 :ml_d2x 0
		:ml_d1y 0 :ml_d2y 0 :ml_slope 0)
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

; 3. AT (animation table: frame sequence playback)
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

; 4. MT (movement table: table of (dx dy) waypoint offsets)
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

; 5. Offscreen (kills sprite when off screen)
(defun offscreen-update (this)
	(bind '(x y w h) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< x 0) (< y 0) (>= (+ x w) ww) (>= (+ y h) wh))
		(. this :kill))
	this)
