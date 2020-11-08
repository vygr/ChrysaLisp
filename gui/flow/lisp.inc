;;;;;;;;;;;;;
; flow widget
;;;;;;;;;;;;;

(class Flow ()
	; (Flow) -> flow
	; overide the default 'this' env with a flow component
	(defq this (create-view))

	(method :pref_size (this)
		; (. flow :pref_size) -> (width height)
		(defq pw 0 ph 0 flow_flags (get :flow_flags this))
		(each (lambda (child)
			(bind '(w h) (view-pref-size child))
			(if (/= 0 (logand flow_flags (const (logior flow_flag_left flow_flag_right))))
				(setq pw (+ pw w)))
			(if (/= 0 (logand flow_flags (const (logior flow_flag_up flow_flag_down))))
				(setq ph (+ ph h)))
			(setq pw (max w pw) ph (max h ph))) (view-children this))
		(list pw ph))

	(method :layout (this)
		; (. flow :layout) -> flow
		(bind '(fw fh) (view-get-size this))
		(defq flow_flags (get :flow_flags this) children (view-children this) end (dec (length children))
			x (if (/= 0 (logand flow_flags (const flow_flag_left))) fw 0)
			y (if (/= 0 (logand flow_flags (const flow_flag_up))) fh 0))
		(each (lambda (child)
			(bind '(cw ch) (view-pref-size child))
			(defq cx x cy y)
			(if (/= 0 (logand flow_flags (const flow_flag_left)))
				(setq cx (- x cw) x cx))
			(if (/= 0 (logand flow_flags (const flow_flag_up)))
				(setq cy (- y ch) y cy))
			(if (/= 0 (logand flow_flags (const flow_flag_right)))
				(setq x (+ x cw)))
			(if (/= 0 (logand flow_flags (const flow_flag_down)))
				(setq y (+ y ch)))

			;filling
			(if (/= 0 (logand flow_flags (const flow_flag_fillw)))
				(setq cx 0 cw fw))
			(if (/= 0 (logand flow_flags (const flow_flag_fillh)))
				(setq cy 0 ch fh))

			;alignment
			(when (/= 0 (logand flow_flags
					(const (logior flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright))))
				(cond
					((/= 0 (logand flow_flags (const flow_flag_align_hleft)))
						(setq cx 0))
					((/= 0 (logand flow_flags (const flow_flag_align_hright)))
						(setq cx (- fw cw)))
					(t	(setq cx (/ (- fw cw) 2)))))
			(when (/= 0 (logand flow_flags
					(const (logior flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom))))
				(cond
					((/= 0 (logand flow_flags (const flow_flag_align_vtop)))
						(setq cy 0))
					((/= 0 (logand flow_flags (const flow_flag_align_vbottom)))
						(setq cy (- fh ch)))
					(t	(setq cy (/ (- fh ch) 2)))))

			;last one ?
			(when (= _ end)
				(when (/= 0 (logand flow_flags (const flow_flag_lastw)))
					(if (/= 0 (logand flow_flags (const flow_flag_right)))
						(setq cw (- fw cx)))
					(if (/= 0 (logand flow_flags (const flow_flag_left)))
						(setq cw (+ cx cw) cx 0)))
				(when (/= 0 (logand flow_flags (const flow_flag_lasth)))
					(if (/= 0 (logand flow_flags (const flow_flag_down)))
						(setq ch (- fh cy)))
					(if (/= 0 (logand flow_flags (const flow_flag_up)))
						(setq ch (+ cy ch) cy 0))))
			(view-change child cx cy cw ch)) children)
		this)
	)
