;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Standard Component Suite
; Small bag of standard reusable components operating on Sprite properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. comp-mv (Movement Vectors: x/y velocity and acceleration)
; Corresponds to C++ class MV (sprite_mv_code)
; Reads/writes sprite properties: :vx :vy :ax :ay :max_vx :max_vy
(defun comp-mv (this action)
	(case action
		(:init
			(def this
				:vx 0 :vy 0 :ax 0 :ay 0 :max_vx 100 :max_vy 100))
		(:update
			(bind '(x y) (. this :get_pos))
			(defq
				ax (get :ax this)
				ay (get :ay this)
				max_vx (get :max_vx this)
				max_vy (get :max_vy this)
				vx (max (neg max_vx) (min max_vx (+ (get :vx this) ax)))
				vy (max (neg max_vy) (min max_vy (+ (get :vy this) ay))))
			(set this :vx vx :vy vy)
			(. this :set_pos (+ x vx) (+ y vy))))
	this)

; 2. comp-ml (Movement Line: straight line Bresenham interpolation / fly-to)
; Corresponds to C++ class ML (sprite_ml_code and ML::sprite_ml_init)
; Reads sprite properties: :target_x :target_y :ml_speed :ml_count :ml_dx :ml_dy :ml_d1x :ml_d2x :ml_d1y :ml_d2y :ml_slope
(defun ml-init (this x y x1 y1 &optional speed)
	; (ml-init sprite x y x1 y1 [speed]) -> sprite
	(when speed (set this :ml_speed speed))
	(. this :set_pos x y)
	(defq
		dx (- x1 x)
		dy (- y1 y)
		d1x (cond ((< dx 0) -1) ((> dx 0) 1) (:t 0))
		d2x d1x
		d1y (cond ((< dy 0) -1) ((> dy 0) 1) (:t 0))
		d2y 0
		abs_dx (abs dx)
		abs_dy (abs dy)
		tmpi 0)
	(when (< abs_dx abs_dy)
		(setq
			d2y d1y
			d2x 0
			tmpi abs_dx
			abs_dx abs_dy
			abs_dy tmpi))
	(set this
		:target_x x1 :target_y y1
		:ml_dx abs_dx
		:ml_dy abs_dy
		:ml_d1x d1x
		:ml_d2x d2x
		:ml_d1y d1y
		:ml_d2y d2y
		:ml_count abs_dx
		:ml_slope (>> abs_dx 1))
	this)

(defun comp-ml (this action)
	(case action
		(:init
			(def this
				:target_x 0 :target_y 0 :ml_speed 0 :ml_count -1
				:ml_dx 0 :ml_dy 0 :ml_d1x 0 :ml_d2x 0
				:ml_d1y 0 :ml_d2y 0 :ml_slope 0))
		(:update
			(defq speed (get :ml_speed this))
			(when (> speed 0)
				(defq
					slope (get :ml_slope this)
					count (get :ml_count this)
					dx (get :ml_dx this)
					dy (get :ml_dy this)
					d1x (get :ml_d1x this)
					d2x (get :ml_d2x this)
					d1y (get :ml_d1y this)
					d2y (get :ml_d2y this))
				(bind '(x y) (. this :get_pos))
				(while (and (> speed 0) (> count 0))
					(-- count)
					(setq slope (- slope dy))
					(if (< slope 0)
						(setq
							slope (+ slope dx)
							x (+ x d1x)
							y (+ y d1y))
						(setq
							x (+ x d2x)
							y (+ y d2y)))
					(-- speed))
				(when (<= count 0)
					(setq count -1
						x (get :target_x this)
						y (get :target_y this)))
				(set this :ml_slope slope :ml_count count)
				(. this :set_pos x y))))
	this)

; 3. comp-at (Animation Table: frame sequence playback)
; Corresponds to C++ class AT (sprite_at_code)
; Reads sprite properties: :at_table :at_speed :at_count :at_index
; Reaching a frame of -1 signals sprite death and triggers removal
(defun comp-at (this action)
	(case action
		(:init
			(def this
				:at_table :nil :at_speed 1 :at_count 0 :at_index 0))
		(:update
			(when (defq table (get :at_table this))
				(defq speed (get :at_speed this))
				(when (> speed 0)
					(defq count (inc (get :at_count this)))
					(if (>= count speed)
						(progn
							(set this :at_count 0)
							(defq idx (get :at_index this)
								f (elem-get table idx))
							(if (= f -1)
								(. this :kill)
								(progn
									(. this :set_frame f)
									(defq next_idx (inc idx))
									(if (>= next_idx (length table))
										(setq next_idx 0))
									(set this :at_index next_idx))))
						(set this :at_count count))))))
	this)

; 4. comp-mt (Movement Table: table of (dx dy) waypoint offsets)
; Corresponds to C++ class MT (sprite_mt_code)
; Reads sprite properties: :mt_table :mt_speed :mt_count :mt_index
(defun comp-mt (this action)
	(case action
		(:init
			(def this
				:mt_table :nil :mt_speed 1 :mt_count 0 :mt_index 0))
		(:update
			(when (defq table (get :mt_table this))
				(defq speed (get :mt_speed this))
				(when (> speed 0)
					(defq count (inc (get :mt_count this)))
					(if (>= count speed)
						(progn
							(set this :mt_count 0)
							(defq idx (get :mt_index this))
							(bind '(dx dy) (elem-get table idx))
							(bind '(x y) (. this :get_pos))
							(. this :set_pos (+ x dx) (+ y dy))
							(defq next_idx (inc idx))
							(if (>= next_idx (length table))
								(setq next_idx 0))
							(set this :mt_index next_idx))
						(set this :mt_count count))))))
	this)

; 5. comp-offscreen (kills sprite when leaving playfield or custom bounds)
; Corresponds to C++ cp_offscreen
; Reads sprite properties: :offscreen_min_x :offscreen_min_y :offscreen_max_x :offscreen_max_y
(defun comp-offscreen (this action)
	(case action
		(:init
			(def this
				:offscreen_min_x -64 :offscreen_max_x (+ +game_width 64)
				:offscreen_min_y -64 :offscreen_max_y (+ +game_height 64)))
		(:update
			(bind '(x y w h) (. this :get_bounds))
			(when (or (< x (get :offscreen_min_x this))
					  (< y (get :offscreen_min_y this))
					  (>= y (get :offscreen_max_y this))
					  (>= x (get :offscreen_max_x this)))
				(. this :kill))))
	this)

; 6. comp-bounds-bounce (bounces within playfield bounds)
; Reads sprite properties: :bounds_min_x :bounds_max_x :bounds_min_y :bounds_max_y
(defun comp-bounds-bounce (this action)
	(case action
		(:init
			(def this
				:bounds_min_x 0 :bounds_max_x +game_width
				:bounds_min_y 0 :bounds_max_y +panel_y))
		(:update
			(bind '(x y w h) (. this :get_bounds))
			(defq
				vx (get :vx this)
				vy (get :vy this)
				min_x (get :bounds_min_x this)
				max_x (- (get :bounds_max_x this) w)
				min_y (get :bounds_min_y this)
				max_y (- (get :bounds_max_y this) h))
			(when (< x min_x)
				(. this :set_bounds min_x y w h)
				(set this :vx (abs vx)))
			(when (> x max_x)
				(. this :set_bounds max_x y w h)
				(set this :vx (neg (abs vx))))
			(when (< y min_y)
				(. this :set_bounds x min_y w h)
				(set this :vy (abs vy)))
			(when (> y max_y)
				(. this :set_bounds x max_y w h)
				(set this :vy (neg (abs vy))))))
	this)

; 7. comp-code (per-tick custom code callback hook)
; Corresponds to C++ CP_CODE
; Reads sprite property: :cp_code
(defun comp-code (this action)
	(case action
		(:init
			(def this :cp_code :nil))
		(:update
			(when (defq fn (get :cp_code this))
				(fn this))))
	this)
