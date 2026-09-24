;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Standard Component Suite
; Small bag of standard reusable components operating on Sprite properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. comp-mv (Movement Vectors: x/y velocity and acceleration)
; Corresponds to C++ class MV (sprite_mv_code)
; Reads/writes sprite properties: :vx :vy :ax :ay :max_vx :max_vy
(defun comp-mv (this dt)
	(bind '(x y w h) (. this :get_bounds))
	(defq
		ax (or (get :ax this) 0)
		ay (or (get :ay this) 0)
		max_vx (or (get :max_vx this) 100)
		max_vy (or (get :max_vy this) 100)
		vx (max (neg max_vx) (min max_vx (+ (or (get :vx this) 0) ax)))
		vy (max (neg max_vy) (min max_vy (+ (or (get :vy this) 0) ay))))
	(def this :vx vx :vy vy)
	(. this :move_to (+ x vx) (+ y vy)))

; 2. comp-ml (Movement Line: straight line interpolation / fly-to)
; Corresponds to C++ class ML (sprite_ml_code)
; Reads sprite properties: :target_x :target_y :ml_speed :ml_step_code :ml_done_code
(defun comp-ml (this dt)
	(defq speed (or (get :ml_speed this) 0))
	(when (> speed 0)
		(bind '(x y w h) (. this :get_bounds))
		(defq
			tx (or (get :target_x this) x)
			ty (or (get :target_y this) y)
			dx (- tx x)
			dy (- ty y)
			dist (max 1 (sqrt (+ (* dx dx) (* dy dy)))))
		(if (<= dist (max 4 speed))
			(progn
				(def this :ml_speed 0)
				(. this :move_to tx ty)
				(when (defq done (get :ml_done_code this))
					(done this)))
			(progn
				(defq
					nx (+ x (/ (* dx speed) dist))
					ny (+ y (/ (* dy speed) dist)))
				(. this :move_to nx ny)
				(when (defq step (get :ml_step_code this))
					(step this))))))

; 3. comp-at (Animation Table: frame sequence playback)
; Corresponds to C++ class AT (sprite_at_code)
; Reads sprite properties: :at_table :at_speed :at_count :at_index
; Reaching a frame of -1 signals sprite death and triggers removal
(defun comp-at (this dt)
	(when (defq table (get :at_table this))
		(defq speed (or (get :at_speed this) 1))
		(when (> speed 0)
			(defq count (inc (or (get :at_count this) 0)))
			(if (>= count speed)
				(progn
					(def this :at_count 0)
					(defq idx (or (get :at_index this) 0)
						f (elem-get table idx))
					(if (= f -1)
						(. this :kill)
						(progn
							(. this :set_frame f)
							(defq next_idx (inc idx))
							(if (>= next_idx (length table))
								(setq next_idx 0))
							(def this :at_index next_idx))))
				(def this :at_count count)))))

; 4. comp-mt (Movement Table: table of (dx dy) waypoint offsets)
; Corresponds to C++ class MT (sprite_mt_code)
; Reads sprite properties: :mt_table :mt_speed :mt_count :mt_index
(defun comp-mt (this dt)
	(when (defq table (get :mt_table this))
		(defq speed (or (get :mt_speed this) 1))
		(when (> speed 0)
			(defq count (inc (or (get :mt_count this) 0)))
			(if (>= count speed)
				(progn
					(def this :mt_count 0)
					(defq idx (or (get :mt_index this) 0))
					(bind '(dx dy) (elem-get table idx))
					(bind '(x y w h) (. this :get_bounds))
					(. this :move_to (+ x dx) (+ y dy))
					(defq next_idx (inc idx))
					(if (>= next_idx (length table))
						(setq next_idx 0))
					(def this :mt_index next_idx))
				(def this :mt_count count)))))

; 5. comp-offscreen (kills sprite when leaving playfield or custom bounds)
; Corresponds to C++ cp_offscreen
; Reads sprite properties: :offscreen_min_x :offscreen_min_y :offscreen_max_x :offscreen_max_y
(defun comp-offscreen (this dt)
	(bind '(x y w h) (. this :get_bounds))
	(defq
		min_x (or (get :offscreen_min_x this) -64)
		min_y (or (get :offscreen_min_y this) -64)
		max_x (or (get :offscreen_max_x this) 384)
		max_y (or (get :offscreen_max_y this) 240))
	(when (or (< x min_x) (< y min_y) (>= y max_y) (>= x max_x))
		(. this :kill)))

; 6. comp-bounds-bounce (bounces within playfield bounds)
; Reads sprite properties: :bounds_min_x :bounds_max_x :bounds_min_y :bounds_max_y
(defun comp-bounds-bounce (this dt)
	(bind '(x y w h) (. this :get_bounds))
	(defq
		vx (or (get :vx this) 0)
		vy (or (get :vy this) 0)
		min_x (or (get :bounds_min_x this) 0)
		max_x (- (or (get :bounds_max_x this) 320) w)
		min_y (or (get :bounds_min_y this) 0)
		max_y (- (or (get :bounds_max_y this) 240) h))
	(when (< x min_x)
		(. this :set_bounds min_x y w h)
		(def this :vx (abs vx)))
	(when (> x max_x)
		(. this :set_bounds max_x y w h)
		(def this :vx (neg (abs vx))))
	(when (< y min_y)
		(. this :set_bounds x min_y w h)
		(def this :vy (abs vy)))
	(when (> y max_y)
		(. this :set_bounds x max_y w h)
		(def this :vy (neg (abs vy)))))

; 7. comp-code (per-tick custom code callback hook)
; Corresponds to C++ CP_CODE
; Reads sprite property: :cp_code
(defun comp-code (this dt)
	(when (defq code (get :cp_code this))
		(code this dt)))
