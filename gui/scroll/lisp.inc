;;;;;;;;;;;;;;;
; scroll widget
;;;;;;;;;;;;;;;

;scroll flags
(defq +scroll_flag_vertical+ 1 +scroll_flag_horizontal+ 2)

;scroll flag combos
(defq +scroll_flag_both+ (logior +scroll_flag_vertical+ +scroll_flag_horizontal+))

(class Scroll (flags)
	; (Scroll flags) -> scroll
	; overide the default 'this' env with a scroll component
	(def (defq this (create-view)) :vslider nil :hslider nil :child nil)
	(when (/= 0 (logand flags +scroll_flag_vertical+))
		(def this :vslider (defq slider (Slider)))
		(component-connect slider (component-get-id this))
		(view-add-front this slider))
	(when (/= 0 (logand flags +scroll_flag_horizontal+))
		(def this :hslider (defq slider (Slider)))
		(component-connect slider (component-get-id this))
		(view-add-front this slider))

	(method :add_child (this child)
		; (. scroll :add_child child) -> scroll
		(if (defq old_child (get :child this))
			(view-sub old_child))
		(set this :child child)
		(view-add-back this child)
		this)

	(method :action (this data)
		; (. scroll :action data) -> scroll
		(. this :layout)
		(view-set-flags this +view_flag_dirty_all+ +view_flag_dirty_all+)
		this)

	(method :pref_size (this)
		; (. scroll :pref_size) -> (width height)
		(defq hslider (get :hslider this) vslider (get :vslider this)
			mw (get :min_width this) mh (get :min_height this))
		(setd mw 0 mh 0)
		(if vslider
			(setq mw (+ mw (elem 0 (view-pref-size vslider)))))
		(if hslider
			(setq mh (+ mh (elem 0 (view-pref-size hslider)))))
		(list mw mh))

	(method :layout (this)
		; (. scroll :layout) -> scroll
		(defq hslider (get :hslider this) vslider (get :vslider this)
			child (get :child this) sw 0 sh 0)

		;position any sliders
		(bind '(w h) (view-get-size this))
		(when vslider
			(bind '(sw _) (view-pref-size vslider))
			(view-change vslider (- w sw) 0 sw h))
		(when hslider
			(bind '(sh _) (view-pref-size hslider))
			(view-change hslider 0 (- h sh) (- w sw) sh))

		;position any child
		(when child
			(defq vval 0 hval 0)
			(bind '(cw ch) (view-get-size child))
			(when vslider
				(unless (setq vval (get :value vslider)) (setq vval 0))
				(defq mo (max 0 (- ch (- h sh))))
				(def vslider :maximum mo :portion (- h sh) :value (setq vval (min vval mo))))
			(when hslider
				(unless (setq hval (get :value hslider)) (setq hval 0))
				(defq mo (max 0 (- cw (- w sw))))
				(def hslider :maximum mo :portion (- w sw) :value (setq hval (min hval mo))))
			(view-set-bounds child (neg hval) (neg vval) cw ch))
		this)
	)
