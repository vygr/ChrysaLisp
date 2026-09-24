;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Map & Terrain Layer
; Loads .dat map & mapflags, directly blits 16x16 tile grid, provides collision queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "./enums.inc")
(import "./sprite.inc")

(unless (def? '*app_root*)
	(defq *app_root* (path-to-file)))

(defq +tile_cols 21) ; 320 / 16 = 20, + 1 for partial scroll
(defq +tile_rows 11) ; 168 / 16 = 10.5 -> 11 rows

(defclass FieldMap () (View)
	; (FieldMap) -> map view
	(def this
		:color 0
		:canvas :nil
		:map_data :nil
		:flags_data :nil
		:map_w +map_width   ; 128
		:map_h +map_height  ; 16
		:scroll_x 0
		:scroll_y 0)
	(. this :set_size (* +map_width +tile_width) (* +map_height +tile_height))

	(defmethod :layout ()
		this)

	(defmethod :load_field_map (&optional map_idx)
		; (map_idx = 1, 2, or 3)
		(setd map_idx 1)
		(defq
			map_file (cat *app_root* "data/fieldmap" (str map_idx) ".dat")
			flags_file (cat *app_root* "data/mapflags1.dat")
			canvas (get-cached-canvas "image/blocks1.cpm"))
		(def this
			:canvas canvas
			:map_data (load map_file)
			:flags_data (load flags_file)
			:map_w +map_width
			:map_h +map_height)
		(. this :set_size (* +map_width +tile_width) (* +map_height +tile_height))
		(. this :dirty)
		this)

	(defmethod :get_tile_flags (x y)
		; Corresponds to C++ map::getmapflags (onslaught.cpp:4177)
		(if (and (<= 0 x (- (* +map_width +tile_width) 1))
				 (<= 0 y (- (* +map_height +tile_height) 1)))
			(if (and (defq md (get :map_data this)) (defq fd (get :flags_data this)))
				(progn
					(defq
						tx (/ x +tile_width)
						ty (/ y +tile_height)
						tile_idx (code (elem-get md (+ tx (* ty (get :map_w this))))))
					(code (elem-get fd tile_idx)))
				+fmap_stand)
			+fmap_stand))

	(defmethod :set_scroll (sx sy)
		(def this :scroll_x sx :scroll_y sy)
		(. this :dirty)
		this)

	(defmethod :draw ()
		; (. map :draw) -> map
		(when (and (defq md (get :map_data this)) (defq c (get :canvas this)))
			(when (defq texture (if (View? c) (getf c +canvas_texture 0) c))
				(bind '(tid tw th) (texture-metrics texture))
				(defq
					map_w (get :map_w this)
					map_h (get :map_h this)
					base_tx (/ *cam_x* 16)
					base_ty (/ *cam_y* 16)
					maxv (max 1 (/ th 16))
					r 0)
				(while (< r +tile_rows)
					(defq
						ty (+ base_ty r)
						c_idx 0)
					(when (and (>= ty 0) (< ty map_h))
						(defq row_offset (* ty map_w))
						(while (< c_idx +tile_cols)
							(defq tx (+ base_tx c_idx))
							(when (and (>= tx 0) (< tx map_w))
								(defq tile_idx (code (elem-get md (+ tx row_offset))))
								(when (/= tile_idx 0)
									(defq
										col_idx (/ tile_idx maxv)
										row_tile (% tile_idx maxv)
										src_x (* col_idx 16)
										src_y (* row_tile 16)
										tile_x (* tx 16)
										tile_y (* ty 16))
									(. this :ctx_blit tid +argb_white tile_x tile_y 16 16 src_x src_y)))
							(setq c_idx (+ c_idx 1))))
					(setq r (+ r 1)))))
		this)
	)

(defmacro ui-map (n &optional p &rest x)
	; (ui-map name [props] [body]) -> map view
	(ui-props p
		:color 0)
	(static-qqp (ui-element ,n (FieldMap) ,p ~x)))
