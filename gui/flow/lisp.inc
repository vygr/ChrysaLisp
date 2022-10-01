;;;;;;;;;;;;;
; flow widget
;;;;;;;;;;;;;

(import "gui/view/lisp.inc")

;flow flags
(bits +flow_flag 0
	(bit left right up down)
	(bit fillw fillh lastw lasth)
	(bit align_hcenter align_hleft align_hright)
	(bit align_vcenter align_vtop align_vbottom))

;useful flow combos
(defq +flow_down_fill (num-intern (logior +flow_flag_down +flow_flag_fillw +flow_flag_lasth))
	+flow_up_fill (num-intern (logior +flow_flag_up +flow_flag_fillw +flow_flag_lasth))
	+flow_right_fill (num-intern (logior +flow_flag_right +flow_flag_fillh +flow_flag_lastw))
	+flow_left_fill (num-intern (logior +flow_flag_left +flow_flag_fillh +flow_flag_lastw))
	+flow_stack_fill (num-intern (logior +flow_flag_fillw +flow_flag_fillh +flow_flag_lastw +flow_flag_lasth)))

(defclass Flow () (View)
	; (Flow) -> flow

	(defmethod :pref_size (this)
		; (. flow :pref_size) -> (width height)
		(defq flow_flags (get :flow_flags this) pw 0 ph 0
			mw (def? :min_width this) mh (def? :min_height this))
		(setd mw 0 mh 0)
		(each (lambda (child)
			(when (= 0 (logand (. child :get_flags) +view_flag_hiden))
				(bind '(w h) (. child :pref_size))
				(if (/= 0 (logand flow_flags (const (num-intern (logior +flow_flag_left +flow_flag_right)))))
					(setq pw (+ pw w)))
				(if (/= 0 (logand flow_flags (const (num-intern (logior +flow_flag_up +flow_flag_down)))))
					(setq ph (+ ph h)))
				(setq pw (max w pw) ph (max h ph)))) (. this :children))
		(list (max pw mw) (max ph mh)))

	(defmethod :layout (this)
		; (. flow :layout) -> flow
		(bind '(fw fh) (. this :get_size))
		(raise :flow_flags (children (. this :children) end (dec (length children))
			x (if (/= 0 (logand flow_flags +flow_flag_left)) fw 0)
			y (if (/= 0 (logand flow_flags +flow_flag_up)) fh 0)))
		(each (lambda (child)
			(cond
				((/= 0 (logand (. child :get_flags) +view_flag_hiden))
					;hiden
					(. child :change 0 0 0 0))
				(:t  ;showing
					(bind '(cw ch) (. child :pref_size))
					(defq cx x cy y)
					(if (/= 0 (logand flow_flags +flow_flag_left))
						(setq cx (- x cw) x cx))
					(if (/= 0 (logand flow_flags +flow_flag_up))
						(setq cy (- y ch) y cy))
					(if (/= 0 (logand flow_flags +flow_flag_right))
						(setq x (+ x cw)))
					(if (/= 0 (logand flow_flags +flow_flag_down))
						(setq y (+ y ch)))

					;filling
					(if (/= 0 (logand flow_flags +flow_flag_fillw))
						(setq cx 0 cw fw))
					(if (/= 0 (logand flow_flags +flow_flag_fillh))
						(setq cy 0 ch fh))

					;alignment
					(when (/= 0 (logand flow_flags
							(const (num-intern (logior +flow_flag_align_hcenter +flow_flag_align_hleft +flow_flag_align_hright)))))
						(cond
							((/= 0 (logand flow_flags +flow_flag_align_hleft))
								(setq cx 0))
							((/= 0 (logand flow_flags +flow_flag_align_hright))
								(setq cx (- fw cw)))
							(:t  (setq cx (/ (- fw cw) 2)))))
					(when (/= 0 (logand flow_flags
							(const (num-intern (logior +flow_flag_align_vcenter +flow_flag_align_vtop +flow_flag_align_vbottom)))))
						(cond
							((/= 0 (logand flow_flags +flow_flag_align_vtop))
								(setq cy 0))
							((/= 0 (logand flow_flags +flow_flag_align_vbottom))
								(setq cy (- fh ch)))
							(:t  (setq cy (/ (- fh ch) 2)))))

					;last one ?
					(when (= _ end)
						(when (/= 0 (logand flow_flags +flow_flag_lastw))
							(if (/= 0 (logand flow_flags +flow_flag_right))
								(setq cw (- fw cx)))
							(if (/= 0 (logand flow_flags +flow_flag_left))
								(setq cw (+ cx cw) cx 0)))
						(when (/= 0 (logand flow_flags +flow_flag_lasth))
							(if (/= 0 (logand flow_flags +flow_flag_down))
								(setq ch (- fh cy)))
							(if (/= 0 (logand flow_flags +flow_flag_up))
								(setq ch (+ cy ch) cy 0))))
					(. child :change cx cy cw ch)))) children)
		this)
	)
