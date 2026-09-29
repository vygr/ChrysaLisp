;;;;;;;;;;;;;;;;;;;;;;;;;;
; layer management helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	*item_inventory* (list +frm_16x16_shield 0 0 0 0 0 0 0)
	*item_usage* (list 48 0 0 0 0 0 0 0)
	*item_selected* 0 *game_mode* +mode_auto *game_frame_count* 0
	*item_select_pressed* :nil *item_drop_cnt* 0
	*panel_item_sprites* (list) *panel_sight_sprite* :nil
	+select_table
	; item priority table matching cpp selecttable (indexed by item - +frm_16x16_gold)
	''(19 9 11 9 12 16 14 17 15 10 18 13 20 20 8 8 8 8 8 8 8 8 8 8))

(defun item-auto-select ()
	; select best item from inventory based on priority table
	(defq best_idx *item_selected* max_pri 0)
	(each (lambda (item_frame)
		(when (> item_frame 0)
			(defq pri (elem-get +select_table (- item_frame +frm_16x16_gold)))
			(when (> pri max_pri)
				(setq max_pri pri best_idx (!)))))
		*item_inventory*)
	(when (> max_pri 0)
		(setq *item_selected* best_idx))
	*item_selected*)

(defun item-select ()
	; cycle selected inventory slot left or right
	; supports dedicated keys (+fkey_keyc / +fkey_keyd) or holding down + left/right
	(defq holding_down (bits? *game_controls* +fkey_down)
		sel_left (or (bits? *game_controls* +fkey_keyc)
					 (and holding_down (bits? *game_controls* +fkey_left)))
		sel_right (or (bits? *game_controls* +fkey_keyd)
					  (and holding_down (bits? *game_controls* +fkey_right))))
	(cond
		(sel_left
			(unless *item_select_pressed*
				(setq *item_select_pressed* :t
					*item_selected* (% (+ *item_selected* 7) 8))))
		(sel_right
			(unless *item_select_pressed*
				(setq *item_select_pressed* :t
					*item_selected* (% (+ *item_selected* 1) 8))))
		(:t
			(setq *item_select_pressed* :nil))))

(defun clear-layer (layer)
	; remove all child views from layer
	(each (# (. %0 :sub)) (cat (. layer :children))))

(defun clear-sprite-layers ()
	; remove all active sprites from all 7 sprite layers
	(each (# (clear-layer %0)) +sprite_layers))

(defun set-world-layers-pos (x y)
	; position all children of *world_layers*
	(each (# (. %0 :set_pos x y)) +world_layers))

(defun set-world-layers-size (w h)
	; size all children of *world_layers*
	(each (# (. %0 :set_size w h)) +world_layers))

(defun update-sprite-canvas (sp)
	(when (def? :sp_canvas_tag sp)
		(case (get :sp_canvas_tag sp)
			(:fanatic
				(set sp :sp_canvas_l *img_fanatic_l* :sp_canvas_r *img_fanatic_r*))
			(:32c32
				(set sp :sp_canvas_l *img_frm_32c32*))
			(:16x16
				(set sp :sp_canvas_l *img_frm_16x16*))
			(:16x16_lr
				(set sp :sp_canvas_l *img_frm_16x16_l* :sp_canvas_r *img_frm_16x16_r*))
			(:16x64
				(set sp :sp_canvas_l *img_frm_16x64*))
			(:32x32_lr
				(set sp :sp_canvas_l *img_frm_32x32_l* :sp_canvas_r *img_frm_32x32_r*))
			(:32c32_lr
				(set sp :sp_canvas_l *img_frm_32c32_l* :sp_canvas_r *img_frm_32c32_r*))
			(:32x16_lr
				(set sp :sp_canvas_l *img_frm_32x16_l* :sp_canvas_r *img_frm_32x16_r*))
			(:64x32_lr
				(set sp :sp_canvas_l *img_frm_64x32_l* :sp_canvas_r *img_frm_64x32_r*))
			(:64x64_lr
				(set sp :sp_canvas_l *img_frm_64x64_l* :sp_canvas_r *img_frm_64x64_r*))
			(:64x64
				(set sp :sp_canvas_l *img_frm_64x64*)))))

(defun rescale-active-sprites ()
	; rescale positions, dimensions and canvases of all live sprites
	(each (lambda (layer)
		(each (lambda (sp)
			(when (Sprite? sp)
				(bind '(x y w h) (. sp :sp_get_bounds))
				(. sp :sp_set_bounds x y w h)
				(update-sprite-canvas sp)))
			(cat (. layer :children))))
		(cat +sprite_layers +panel_layers)))

(defun update-sprite-layers ()
	; tick all active sprites across world layers
	(each (# (each (lambda (sp) (when (Sprite? sp) (. sp :sp_update))) (cat (. %0 :children))))
		+sprite_layers))

(defun check-empty-mase ()
	; if player has no offensive weapons or attack spells, grant 6 mace swings in first free slot
	(when (= (logand (++ *game_frame_count*) 127) 0)
		(unless (some (# (and (> %0 0) (< %0 (const (+ +frm_16x16_spells 3))))) *item_inventory*)
			(defq empty_idx (find 0 *item_inventory*))
			(when empty_idx
				(elem-set *item_inventory* empty_idx +frm_16x16_shield)
				(elem-set *item_usage* empty_idx 6)
				(when (= *game_mode* +mode_auto)
					(item-auto-select))))))

(defun get-xy-outer (w h)
	; pick a random start point just outside the playable area
	(case (random 4)
		(0 (list (- (random (+ +window_width (* 2 w))) w) (neg h)))
		(1 (list (- (random (+ +window_width (* 2 w))) w) +window_height))
		(2 (list (neg w) (- (random (+ +window_height (* 2 h))) h)))
		(3 (list +window_width (- (random (+ +window_height (* 2 h))) h)))))

(defun update-camera (sprite)
	; center camera on sprite using feet baseline anchor (+ y h -16)
	(bind '(x y w h) (. sprite :sp_get_bounds))
	(defq cam_x (- (+ x (/ w 2)) (const (/ +window_width 2)))
		cam_y (- (+ y h -16) (const (/ +window_height 2)))
		max_cam_x (- (* +map_width +tile_width) +window_width)
		max_cam_y (- (* +map_height +tile_height) +window_height)
		cx (max 0 (min max_cam_x cam_x))
		cy (max 0 (min max_cam_y cam_y)))
	; scroll all world layers by negative camera offset
	(set-world-layers-pos
		(* *zoom* (neg cx))
		(* *zoom* (neg cy))))

(defun update-panel-status ()
	; update enemy stack flags and power/strength level indicators directly
	(defq stack_val (max 0 (min 32 (* (or *enemy_stack_index* 0) 2)))
		flag_y (+ +status_flag_y (- 32 stack_val))
		flag_oy (- 32 stack_val)
		power_val (max 0 (min 32 (/ (* (max 0 (min +max_power (or *man_power* 0))) 32) +max_power)))
		power_y (+ +status_level_y (- 32 power_val))
		power_oy (- 32 power_val)
		strength_val (max 0 (min 32 (/ (* (max 0 (min +max_strength (or *man_strength* 0))) 32) +max_strength)))
		strength_y (+ +status_level_y (- 32 strength_val))
		strength_oy (- 32 strength_val))
	(def *layer_panel_items* :canvas *img_frm_16x16* :zoom *zoom*
		:items *item_inventory* :selected *item_selected*)
	(.-> *panel_flag_l*
		(:sp_set_bounds +status_flag_l_x flag_y 32 stack_val)
		(:sp_set_oy flag_oy))
	(.-> *panel_flag_r*
		(:sp_set_bounds +status_flag_r_x flag_y 32 stack_val)
		(:sp_set_oy flag_oy))
	(.-> *panel_power*
		(:sp_set_bounds +status_power_x power_y 32 power_val)
		(:sp_set_oy power_oy))
	(.-> *panel_strength*
		(:sp_set_bounds +status_strength_x strength_y 32 strength_val)
		(:sp_set_oy strength_oy)))

(defun update-panel-layers ()
	; tick all active sprites across panel layers
	(check-empty-mase)
	(item-select)
	(update-panel-status)
	(each (# (each (lambda (sp) (when (Sprite? sp) (. sp :sp_update))) (cat (. %0 :children))))
		+panel_layers))

(defun update-frame ()
	; common screen and layer updates across all game states
	(update-sprite-layers)
	(update-panel-layers)
	(. *world_scroll* :dirty_all)
	(. *panel_layers* :dirty_all))
