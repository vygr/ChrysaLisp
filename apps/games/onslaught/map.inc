;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught field terrain map renderer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass FieldMap () (View)
	; (FieldMap) -> map view
	(def this :color 0 :map_idx 1 :blocks_canvas :nil :map_data :nil :flags_data :nil)
	(. this :set_size (const (* +map_width +tile_width)) (const (* +map_height +tile_height)))

	(defmethod :clear ()
		; (. map :clear) -> map
		(def this :map_data :nil :flags_data :nil :blocks_canvas :nil)
		this)

	(defmethod :load_field_map (&optional map_idx)
		; (. map :load_field_map [map_idx]) -> map
		(setd map_idx (or (get :map_idx this) 1))
		(defq blocks_canvas (cond
				((= map_idx 2) *img_blocks2*)
				((= map_idx 3) *img_blocks3*)
				(:t *img_blocks1*))
			app_root (or (get '*app_root*) "")
			map_data (load (cat app_root "data/fieldmap" (str map_idx) ".dat"))
			flags_data (load (cat app_root "data/mapflags1.dat")))
		(lower :map_idx :map_data :flags_data :blocks_canvas)
		this)

	(defmethod :get_tile_flags (x y)
		; (. map :get_tile_flags x y) -> flags
		(if (and (< -1 x (const (* +map_width +tile_width))) (< -1 y (const (* +map_height +tile_height))))
			(if (and (defq md (get :map_data this)) (defq fd (get :flags_data this)))
				(code fd 1 (code md 1 (+ (/ x +tile_width) (* (/ y +tile_height) +map_width))))
				+fmap_stand)
			+fmap_stand))

	(defmethod :draw ()
		; (. map :draw) -> map
		(when (and (defq md (get :map_data this))
				(defq c (get :blocks_canvas this))
				(defq texture (getf c +canvas_texture 0)))
			(bind '(tid tw th) (texture-metrics texture))
			(bind '(vx vy) (. this :get_pos))
			(bind '(vw vh) (. (penv this) :get_size))
			(defq zoom (get :zoom this) vx (neg vx) vy (neg vy)
				tile_w (* zoom +tile_width) tile_h (* zoom +tile_height)
				tx_start (max 0 (/ vx tile_w)) ty (max 0 (/ vy tile_h))
				tx_end (min +map_width (+ (/ (+ vx vw) tile_w) 1))
				ty_end (min +map_height (+ (/ (+ vy vh) tile_h) 1))
				maxv (/ th tile_h))
			(while (< ty ty_end)
				(defq row_offset (* ty +map_width) tile_y (* ty tile_h) tx tx_start)
				(while (< tx tx_end)
					(defq tile_idx (code md 1 (+ tx row_offset)))
					(when (/= tile_idx 0)
						(defq col_idx (/ tile_idx maxv) row_tile (% tile_idx maxv)
							src_x (* col_idx tile_w) src_y (* row_tile tile_h)
							tile_x (* tx tile_w))
						(. this :ctx_blit tid +argb_white tile_x tile_y tile_w tile_h src_x src_y))
					(++ tx))
				(++ ty)))
			this)
	)

(defmacro ui-map (n &optional p &rest x)
	; (ui-map name [props] [body]) -> map view
	(ui-props p
		:color 0)
	(static-qqp (ui-element ,n (FieldMap) ,p ~x)))