;Animation system for Fractal Explorer Phase 3

;Animation mode enumeration (from config.inc)
;+anim_none +anim_zoom_in +anim_zoom_out +anim_rotate_params +anim_explore

(defq +anim_zoom_in_steps 60
	+anim_zoom_out_steps 60
	+anim_rotate_steps 120
	+anim_explore_steps 200)

;Animation state structure
(defun anim-create ()
	(scatter (Emap)
		:mode +anim_none
		:current_step 0
		:total_steps 0
		:start_cx 0
		:start_cy 0
		:start_zoom 0
		:start_p1 0
		:start_p2 0
		:end_cx 0
		:end_cy 0
		:end_zoom 0
		:end_p1 0
		:end_p2 0
		:recording :nil
		:frame_count 0))

;Start zoom in animation
(defun anim-start-zoom-in (state cx cy zoom)
	(def state
		:mode +anim_zoom_in
		:current_step 0
		:total_steps +anim_zoom_in_steps
		:start_cx cx
		:start_cy cy
		:start_zoom zoom
		:end_cx cx
		:end_cy cy
		:end_zoom (mbfp-mul zoom (mbfp-from-fixed 0.1)))
	state)

;Start zoom out animation
(defun anim-start-zoom-out (state cx cy zoom)
	(def state
		:mode +anim_zoom_out
		:current_step 0
		:total_steps +anim_zoom_out_steps
		:start_cx cx
		:start_cy cy
		:start_zoom zoom
		:end_cx cx
		:end_cy cy
		:end_zoom (mbfp-mul zoom (mbfp-from-fixed 10.0)))
	state)

;Start parameter rotation animation (for Julia sets)
(defun anim-start-rotate-params (state p1 p2)
	(def state
		:mode +anim_rotate_params
		:current_step 0
		:total_steps +anim_rotate_steps
		:start_p1 p1
		:start_p2 p2)
	state)

;Start auto-explore animation
(defun anim-start-explore (state cx cy zoom)
	(def state
		:mode +anim_explore
		:current_step 0
		:total_steps +anim_explore_steps
		:start_cx cx
		:start_cy cy
		:start_zoom zoom)
	state)

;Stop animation
(defun anim-stop (state)
	(def state :mode +anim_none :current_step 0)
	state)

;Get current animation frame parameters
(defun anim-get-params (state)
	(defq mode (. state :find :mode)
		step (. state :find :current_step)
		total (. state :find :total_steps)
		progress (if (> total 0) (/ step total) 0))

	(case mode
		(+anim_zoom_in
			;smooth zoom in with easing
			(defq ease_progress (- 1.0 (* (- 1.0 progress) (- 1.0 progress))))
			(defq start_zoom (. state :find :start_zoom))
			(defq end_zoom (. state :find :end_zoom))
			(defq current_zoom (+ start_zoom (mbfp-mul (- end_zoom start_zoom) (<< (fix (* ease_progress 1000000)) (- +mbfp_shift 20)))))
			(list :zoom current_zoom))

		(+anim_zoom_out
			;smooth zoom out with easing
			(defq ease_progress (- 1.0 (* (- 1.0 progress) (- 1.0 progress))))
			(defq start_zoom (. state :find :start_zoom))
			(defq end_zoom (. state :find :end_zoom))
			(defq current_zoom (+ start_zoom (mbfp-mul (- end_zoom start_zoom) (<< (fix (* ease_progress 1000000)) (- +mbfp_shift 20)))))
			(list :zoom current_zoom))

		(+anim_rotate_params
			;rotate Julia parameters in a circle
			(defq angle (* progress 6.28318))
			(defq radius (mbfp-from-fixed 0.7))
			(defq p1 (mbfp-mul radius (<< (fix (* (cos angle) 1000000)) (- +mbfp_shift 20))))
			(defq p2 (mbfp-mul radius (<< (fix (* (sin angle) 1000000)) (- +mbfp_shift 20))))
			(list :param1 p1 :param2 p2))

		(+anim_explore
			;random walk with zoom
			(defq angle (* step 0.1))
			(defq start_cx (. state :find :start_cx))
			(defq start_cy (. state :find :start_cy))
			(defq start_zoom (. state :find :start_zoom))
			(defq offset_x (mbfp-mul start_zoom (<< (fix (* (sin angle) 500000)) (- +mbfp_shift 20))))
			(defq offset_y (mbfp-mul start_zoom (<< (fix (* (cos angle) 500000)) (- +mbfp_shift 20))))
			(defq zoom_factor (+ 1.0 (* 0.5 (sin (* step 0.05)))))
			(list
				:center_x (+ start_cx offset_x)
				:center_y (+ start_cy offset_y)
				:zoom (mbfp-mul start_zoom (<< (fix (* zoom_factor 1000000)) (- +mbfp_shift 20)))))

		(:t (list))))

;Advance animation by one frame
(defun anim-step (state)
	(defq mode (. state :find :mode))
	(when (/= mode +anim_none)
		(def state :current_step (inc (. state :find :current_step)))
		;check if animation is complete
		(when (>= (. state :find :current_step) (. state :find :total_steps))
			;loop animation
			(def state :current_step 0)))
	state)

;Check if animation is active
(defun anim-active? (state)
	(/= (. state :find :mode) +anim_none))

;Get animation progress (0.0 to 1.0)
(defun anim-progress (state)
	(defq total (. state :find :total_steps))
	(if (> total 0)
		(/ (. state :find :current_step) total)
		0.0))

;Get animation mode name
(defun anim-mode-name (state)
	(case (. state :find :mode)
		(+anim_zoom_in "Zooming In")
		(+anim_zoom_out "Zooming Out")
		(+anim_rotate_params "Rotating Parameters")
		(+anim_explore "Exploring")
		(:t "None")))
