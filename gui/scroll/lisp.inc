;;;;;;;;;;;;;;;
; scroll widget
;;;;;;;;;;;;;;;

(import "././slider/lisp.inc")

;scroll flags
(bits +scroll_flag 0
	(bit vertical horizontal))

;scroll flag combos
(defq +scroll_flag_both (bit-mask +scroll_flag_vertical +scroll_flag_horizontal))

(defclass Scroll (flags) (View)
	; (Scroll flags) -> scroll
	(def this :vslider :nil :hslider :nil :child :nil)
	(when (bits? flags +scroll_flag_vertical)
		(def this :vslider (defq slider (Slider)))
		(. slider :connect (. this :get_id))
		(. this :add_front slider))
	(when (bits? flags +scroll_flag_horizontal)
		(def this :hslider (defq slider (Slider)))
		(. slider :connect (. this :get_id))
		(. this :add_front slider))

	(defmethod :add_child (child)
		; (. scroll :add_child child) -> scroll
		(. child :set_flags +view_flag_subtree +view_flag_subtree)
		(if (defq old_child (get :child this))
			(. old_child :sub))
		(. this :add_back (set this :child child)))

	(defmethod :action (data)
		; (. scroll :action data) -> scroll
		(.-> this :layout (:set_flags +view_flag_dirty_all +view_flag_dirty_all)))

	(defmethod :constraint ()
		; (. scroll :constraint) -> (width height)
		(defq hslider (get :hslider this) vslider (get :vslider this)
			mw (ifn (def? :min_width this) 0) mh (ifn (def? :min_height this) 0))
		(if vslider (setq mw (+ mw (pop (. vslider :get_constraint)))))
		(if hslider (setq mh (+ mh (pop (. hslider :get_constraint)))))
		(list mw mh))

	(defmethod :layout ()
		; (. scroll :layout) -> scroll
		(defq hslider (get :hslider this) vslider (get :vslider this)
			child (get :child this) sw 0 sh 0)

		;position any sliders
		(bind '(w h) (. this :get_size))
		(when vslider
			(bind '(sw _) (. vslider :get_constraint))
			(. vslider :set_bounds (- w sw) 0 sw h))
		(when hslider
			(bind '(sh _) (. hslider :get_constraint))
			(. hslider :set_bounds 0 (- h sh) (- w sw) sh))

		;position any child
		(when child
			(defq vval 0 hval 0)
			(bind '(cw ch) (. child :get_size))
			(when vslider
				(unless (setq vval (get :value vslider)) (setq vval 0))
				(defq mo (max 0 (- ch (- h sh))))
				(def vslider :maximum mo :portion (- h sh) :value (setq vval (max 0 (min vval mo)))))
			(when hslider
				(unless (setq hval (get :value hslider)) (setq hval 0))
				(defq mo (max 0 (- cw (- w sw))))
				(def hslider :maximum mo :portion (- w sw) :value (setq hval (max 0 (min hval mo)))))
			(. child :set_bounds (neg hval) (neg vval) cw ch))
		this)

	(defmethod :visible (descendant)
		; (. scroll :visible descendant) -> scroll
		(raise :hslider :vslider :child (flag :nil))
		(when child
			(bind '(x y w h) (. child :get_relative descendant))
			(bind '(sw sh) (. this :get_size))
			(if vslider (setq sw (- sw (first (. vslider :get_size)))))
			(if hslider (setq sh (- sh (second (. hslider :get_size)))))
			(when hslider
				(defq val (get :value hslider))
				(when (< x val)
					(def hslider :value x)
					(setq flag :t))
				(when (> (+ x w) (+ val sw))
					(def hslider :value (- (+ x w) sw))
					 (setq flag :t)))
			(when vslider
				(defq val (get :value vslider))
				(when (< y val)
					(def vslider :value y)
					 (setq flag :t))
				(when (> (+ y h) (+ val sh))
					(def vslider :value (- (+ y h) sh))
					 (setq flag :t))))
		(if flag (.-> this :layout :dirty_all) this))

	(defmethod :mouse_wheel (event)
		; (. scroll :mouse_wheel event) -> scroll
		(raise :hslider :vslider)
		(if hslider (def hslider :value (+ (get :value hslider) (* 16 (getf event +ev_msg_wheel_x)))))
		(if vslider (def vslider :value (- (get :value vslider) (* 16 (getf event +ev_msg_wheel_y)))))
		;dispatch wheel events to parent if not used
		(unless hslider
			(defq target (penv this))
			(while (and target (not (defq fnc (.? target :mouse_wheel))))
				(setq target (penv target)))
			(if target (fnc target (setf (cat event) +ev_msg_wheel_y 0))))
		(unless vslider
			(defq target (penv this))
			(while (and target (not (defq fnc (.? target :mouse_wheel))))
				(setq target (penv target)))
			(if target (fnc target (setf (cat event) +ev_msg_wheel_x 0))))
		(. this :action event))
	)
