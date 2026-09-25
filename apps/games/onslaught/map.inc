;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/map.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught field terrain map renderer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "gui/canvas/lisp.inc")
(import "./enums.inc")

(defclass FieldMap () (View)
	; (FieldMap) -> map view
	(def this
		:color 0
		:map_idx 1
		:blocks_canvas :nil
		:map_data :nil
		:flags_data :nil
		:tile_w 16
		:tile_h 16
		:map_w +map_width
		:map_h +map_height)
	(. this :set_size (* +map_width +tile_width) (* +map_height +tile_height))

	(defmethod :clear ()
		; (. map :clear) -> map
		(def this :map_data :nil :flags_data :nil :blocks_canvas :nil)
		(. this :dirty)
		this)

	(defmethod :set_blocks_canvas (c zoom)
		; (. map :set_blocks_canvas canvas zoom) -> map
		(def this
			:blocks_canvas c
			:tile_w (* zoom +tile_width)
			:tile_h (* zoom +tile_height))
		(. this :set_size (* +map_width (get :tile_w this)) (* +map_height (get :tile_h this)))
		(. this :dirty)
		this)

	(defmethod :load_field_map (&optional map_idx)
		; (map_idx = 1, 2, or 3)
		(setd map_idx (or (get :map_idx this) 1))
		(defq
			zoom (or (get '*zoom*) 1)
			canvas (cond
				((= map_idx 2) *img_blocks2*)
				((= map_idx 3) *img_blocks3*)
				(:t *img_blocks1*))
			app_root (or (get '*app_root*) "")
			map_file (cat app_root "data/fieldmap" (str map_idx) ".dat")
			flags_file (cat app_root "data/mapflags1.dat"))
		(def this
			:map_idx map_idx
			:map_data (load map_file)
			:flags_data (load flags_file)
			:map_w +map_width
			:map_h +map_height)
		(. this :set_blocks_canvas canvas zoom)
		this)

	(defmethod :get_tile_flags (x y)
		; corresponds to c++ map::getmapflags
		(defq
			tile_w (or (get :tile_w this) 16)
			tile_h (or (get :tile_h this) 16)
			map_w (or (get :map_w this) +map_width)
			map_h (or (get :map_h this) +map_height))
		(if (and (<= 0 x (- (* map_w tile_w) 1))
				 (<= 0 y (- (* map_h tile_h) 1)))
			(if (and (defq md (get :map_data this)) (defq fd (get :flags_data this)))
				(progn
					(defq
						tx (/ x tile_w)
						ty (/ y tile_h)
						tile_idx (code md 1 (+ tx (* ty map_w))))
					(code fd 1 tile_idx))
				+fmap_stand)
			+fmap_stand))

	(defmethod :draw ()
		; (. map :draw) -> map
		(when (and (defq md (get :map_data this))
				   (defq c (get :blocks_canvas this)))
			(when (defq texture (if (View? c) (getf c +canvas_texture 0) c))
				(bind '(tid tw th) (texture-metrics texture))
				(bind '(vx vy) (. this :get_pos))
				(bind '(vw vh) (. (penv this) :get_size))
				(defq
					tile_w (or (get :tile_w this) 16)
					tile_h (or (get :tile_h this) 16)
					map_w (or (get :map_w this) +map_width)
					map_h (or (get :map_h this) +map_height)
					cam_x (neg vx)
					cam_y (neg vy)
					tx_start (max 0 (/ cam_x tile_w))
					tx_end (min map_w (+ (/ (+ cam_x vw) tile_w) 2))
					ty_start (max 0 (/ cam_y tile_h))
					ty_end (min map_h (+ (/ (+ cam_y vh) tile_h) 2))
					maxv (max 1 (/ th tile_h))
					ty ty_start)
				(while (< ty ty_end)
					(defq
						row_offset (* ty map_w)
						tile_y (* ty tile_h)
						tx tx_start)
					(while (< tx tx_end)
						(defq tile_idx (code md 1 (+ tx row_offset)))
						(when (/= tile_idx 0)
							(defq
								col_idx (/ tile_idx maxv)
								row_tile (% tile_idx maxv)
								src_x (* col_idx tile_w)
								src_y (* row_tile tile_h)
								tile_x (* tx tile_w))
							(. this :ctx_blit tid +argb_white tile_x tile_y tile_w tile_h src_x src_y))
						(setq tx (+ tx 1)))
					(setq ty (+ ty 1)))))
		this)
	)

(defmacro ui-map (n &optional p &rest x)
	; (ui-map name [props] [body]) -> map view
	(ui-props p
		:color 0)
	(static-qqp (ui-element ,n (FieldMap) ,p ~x)))