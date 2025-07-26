;;;;;;;;;;;;;
; flow widget
;;;;;;;;;;;;;

(import "././view/lisp.inc")

;flow flags
(bits +flow_flag 0
	(bit left right up down)
	(bit fillw fillh lastw lasth)
	(bit align_hcenter align_hleft align_hright)
	(bit align_vcenter align_vtop align_vbottom))

;useful flow combos
(defq +flow_down (bit-mask +flow_flag_down +flow_flag_fillw)
	+flow_up (bit-mask +flow_flag_up +flow_flag_fillw)
	+flow_right (bit-mask +flow_flag_right +flow_flag_fillh)
	+flow_left (bit-mask +flow_flag_left +flow_flag_fillh)
	+flow_left_right (bit-mask +flow_flag_left +flow_flag_right)
	+flow_up_down (bit-mask +flow_flag_up +flow_flag_down)
	+flow_down_fill (bit-mask +flow_flag_down +flow_flag_fillw +flow_flag_lasth)
	+flow_up_fill (bit-mask +flow_flag_up +flow_flag_fillw +flow_flag_lasth)
	+flow_right_fill (bit-mask +flow_flag_right +flow_flag_fillh +flow_flag_lastw)
	+flow_left_fill (bit-mask +flow_flag_left +flow_flag_fillh +flow_flag_lastw)
	+flow_stack_fill (bit-mask +flow_flag_fillw +flow_flag_fillh +flow_flag_lastw +flow_flag_lasth))

(defclass Flow () (View)
	; (Flow) -> flow

	(defmethod :constraint ()
		; (. flow :constraint) -> (width height)
		(raise :flow_flags (pw 0 ph 0
			flow_lr (bits? flow_flags +flow_left_right)
			flow_ud (bits? flow_flags +flow_up_down)))
		(each (lambda (child)
			(unless (bits? (. child :get_flags) +view_flag_hidden)
				(bind '(w h) (. child :get_constraint))
				(if flow_lr (++ pw w))
				(if flow_ud (++ ph h))
				(setq pw (max w pw) ph (max h ph)))) (. this :children))
		(list (max pw (ifn (def? :min_width this) 0))
			(max ph (ifn (def? :min_height this) 0))))

	(defmethod :layout ()
		; (. flow :layout) -> flow
		(bind '(fw fh) (. this :get_size))
		(raise :flow_flags (children (. this :children) end (dec (length children))
			x (if (bits? flow_flags +flow_flag_left) fw 0)
			y (if (bits? flow_flags +flow_flag_up) fh 0)))
		(each (lambda (child)
			(cond
				((bits? (. child :get_flags) +view_flag_hidden)
					;hidden
					(. child :set_bounds 0 0 0 0))
				(:t ;showing
					(bind '(cw ch) (. child :get_constraint))
					(defq cx x cy y)
					(if (bits? flow_flags +flow_flag_left)
						(setq cx (- x cw) x cx))
					(if (bits? flow_flags +flow_flag_up)
						(setq cy (- y ch) y cy))
					(if (bits? flow_flags +flow_flag_right)
						(++ x cw))
					(if (bits? flow_flags +flow_flag_down)
						(++ y ch))

					;filling
					(if (bits? flow_flags +flow_flag_fillw)
						(setq cx 0 cw fw))
					(if (bits? flow_flags +flow_flag_fillh)
						(setq cy 0 ch fh))

					;alignment
					(when (bits? flow_flags +flow_flag_align_hcenter +flow_flag_align_hleft +flow_flag_align_hright)
						(cond
							((bits? flow_flags +flow_flag_align_hleft)
								(setq cx 0))
							((bits? flow_flags +flow_flag_align_hright)
								(setq cx (- fw cw)))
							(:t (setq cx (/ (- fw cw) 2)))))
					(when (bits? flow_flags +flow_flag_align_vcenter +flow_flag_align_vtop +flow_flag_align_vbottom)
						(cond
							((bits? flow_flags +flow_flag_align_vtop)
								(setq cy 0))
							((bits? flow_flags +flow_flag_align_vbottom)
								(setq cy (- fh ch)))
							(:t (setq cy (/ (- fh ch) 2)))))

					;last one ?
					(when (= (!) end)
						(when (bits? flow_flags +flow_flag_lastw)
							(if (bits? flow_flags +flow_flag_right)
								(setq cw (- fw cx)))
							(if (bits? flow_flags +flow_flag_left)
								(setq cw (+ cx cw) cx 0)))
						(when (bits? flow_flags +flow_flag_lasth)
							(if (bits? flow_flags +flow_flag_down)
								(setq ch (- fh cy)))
							(if (bits? flow_flags +flow_flag_up)
								(setq ch (+ cy ch) cy 0))))
					(. child :set_bounds cx cy cw ch)))) children)
		this)
	)
