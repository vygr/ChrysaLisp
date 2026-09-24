;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Core Reusable 2D Components for Sprite Entity System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comp-fly-to (this action &rest args)
	(case action
		(:init
			(def this :target_x 0 :target_y 0 :speed 16 :on_arrive :nil :arrived :nil)
			(while (nempty? args)
				(case (pop args)
					(:target_x (def this :target_x (pop args)))
					(:target_y (def this :target_y (pop args)))
					(:speed (def this :speed (pop args)))
					(:on_arrive (def this :on_arrive (pop args))))))
		(:update
			(bind '(dt) args)
			(unless (get :arrived this)
				(bind '(x y w h) (. this :get_bounds))
				(defq tx (get :target_x this) ty (get :target_y this)
					dx (- tx x) dy (- ty y)
					dist (max 1 (sqrt (+ (* dx dx) (* dy dy))))
					step (get :speed this))
				(if (<= dist (max 4 step))
					(progn
						(def this :arrived :t)
						(. this :move_to tx ty)
						(when (defq cb (get :on_arrive this))
							(def this :on_arrive :nil)
							(catch (cb this) (cb))))
					(defq new_x (+ x (/ (* dx step) dist))
						new_y (+ y (/ (* dy step) dist)))
					(. this :move_to new_x new_y))))))

(defun comp-drop (this action &rest args)
	(case action
		(:init
			(def this :target_y 88 :drop_speed 10 :on_arrive :nil :arrived :nil)
			(while (nempty? args)
				(case (pop args)
					(:target_y (def this :target_y (pop args)))
					(:drop_speed (def this :drop_speed (pop args)))
					(:on_arrive (def this :on_arrive (pop args))))))
		(:update
			(bind '(dt) args)
			(unless (get :arrived this)
				(bind '(x y w h) (. this :get_bounds))
				(defq ty (get :target_y this)
					step (get :drop_speed this)
					new_y (+ y step))
				(if (>= new_y ty)
					(progn
						(def this :arrived :t)
						(. this :move_to x ty)
						(when (defq cb (get :on_arrive this))
							(def this :on_arrive :nil)
							(catch (cb this) (cb))))
					(. this :move_to x new_y))))))

(defun comp-blood-drip (this action &rest args)
	(case action
		(:init
			(def this :drip_speed 2 :drip_start_y 116 :drip_max_y 168 :on_drip :nil :on_finish :nil :drips_left 1 :drip_done :nil)
			(while (nempty? args)
				(case (pop args)
					(:drip_speed (def this :drip_speed (pop args)))
					(:drip_start_y (def this :drip_start_y (pop args)))
					(:drip_max_y (def this :drip_max_y (pop args)))
					(:drips_left (def this :drips_left (pop args)))
					(:on_drip (def this :on_drip (pop args)))
					(:on_finish (def this :on_finish (pop args))))))
		(:update
			(bind '(dt) args)
			(unless (get :drip_done this)
				(bind '(x y w h) (. this :get_bounds))
				(defq new_y (+ y (get :drip_speed this)))
				(if (>= new_y (get :drip_max_y this))
					(progn
						(when (defq cb (get :on_drip this)) (catch (cb this) (cb)))
						(defq left (dec (get :drips_left this)))
						(def this :drips_left left)
						(if (<= left 0)
							(progn
								(def this :drip_done :t)
								(. this :move_to x (get :drip_max_y this))
								(when (defq fcb (get :on_finish this)) (catch (fcb this) (fcb))))
							(. this :move_to x (get :drip_start_y this))))
					(. this :move_to x new_y))))))

(defun comp-motion (this action &rest args)
	(case action
		(:init
			(def this :vx 0 :vy 0)
			(while (nempty? args)
				(case (pop args)
					(:vx (def this :vx (pop args)))
					(:vy (def this :vy (pop args))))))
		(:update
			(bind '(dt) args)
			(bind '(x y w h) (. this :get_bounds))
			(defq vx (get :vx this) vy (get :vy this))
			(when (or (/= vx 0) (/= vy 0))
				(. this :move_to (+ x vx) (+ y vy))))))

(defun comp-bounds-bounce (this action &rest args)
	(case action
		(:init
			(def this :bounds_min_x 0 :bounds_max_x 320 :bounds_min_y 0 :bounds_max_y 240)
			(while (nempty? args)
				(case (pop args)
					(:bounds_min_x (def this :bounds_min_x (pop args)))
					(:bounds_max_x (def this :bounds_max_x (pop args)))
					(:bounds_min_y (def this :bounds_min_y (pop args)))
					(:bounds_max_y (def this :bounds_max_y (pop args))))))
		(:update
			(bind '(dt) args)
			(bind '(x y w h) (. this :get_bounds))
			(defq vx (get :vx this) vy (get :vy this)
				min_x (get :bounds_min_x this) max_x (- (get :bounds_max_x this) w)
				min_y (get :bounds_min_y this) max_y (- (get :bounds_max_y this) h))
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
				(def this :vy (neg (abs vy)))))))

(defun comp-anim (this action &rest args)
	(case action
		(:init
			(def this :anim_timer 0 :anim_speed 3 :anim_start 0 :anim_count 1)
			(while (nempty? args)
				(case (pop args)
					(:anim_timer (def this :anim_timer (pop args)))
					(:anim_speed (def this :anim_speed (pop args)))
					(:anim_start (def this :anim_start (pop args)))
					(:anim_count (def this :anim_count (pop args))))))
		(:update
			(bind '(dt) args)
			(defq count (get :anim_count this))
			(when (> count 1)
				(defq timer (inc (get :anim_timer this))
					speed (get :anim_speed this))
				(if (>= timer speed)
					(progn
						(def this :anim_timer 0)
						(defq cur (- (get :frame this) (get :anim_start this))
							next_f (+ (get :anim_start this) (% (inc cur) count)))
						(. this :set_frame next_f))
					(def this :anim_timer timer))))))
