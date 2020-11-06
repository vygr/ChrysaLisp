;;;;;;;;;;;;;;;
; scroll widget
;;;;;;;;;;;;;;;

(ffi create-scroll "gui/scroll/lisp_create" 0)

(class scroll (flags)
	; (scroll) -> scroll
	; overide the default 'this' env with a scroll component
	(defq this (create-scroll flags))

	(method :layout (this)
		; (. scroll :layout) -> scroll
		(defq hslider (get-field this (const scroll_hslider) 0)
			vslider (get-field this (const scroll_vslider) 0)
			child (get-field this (const scroll_child) 0)
			sw 0 sh 0)

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
