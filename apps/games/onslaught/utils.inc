;;;;;;;;;;;;;;;;;;;;;;;;;;
; layer management helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	*item_inventory* (list +frm_16x16_shield 0 0 0 0 0 0 0)
	*item_usage* (list 48 0 0 0 0 0 0 0)
	*item_selected* 0
	*item_select_pressed* :nil
	*item_drop_cnt* 0
	*panel_item_sprites* (list)
	*panel_sight_sprite* :nil)

(defun init-panel-items ()
	; create 8 slot sprites and 1 selector sight sprite in *layer_panel_items*
	(clear-layer *layer_panel_items*)
	(setq *panel_item_sprites* (list))
	(for 0 8
		(defq sp (. (Sprite *img_frm_16x16* :nil 16 16 :16x16) :sp_set_frame -1))
		(. sp :sp_set_pos (* (!) 16) 0)
		(. *layer_panel_items* :add_front sp)
		(push *panel_item_sprites* sp))
	(defq sight (. (Sprite *img_frm_16x16* :nil 16 16 :16x16) :sp_set_frame +frm_16x16_sight))
	(. sight :sp_set_pos (* *item_selected* 16) 0)
	(. *layer_panel_items* :add_front sight)
	(setq *panel_sight_sprite* sight)
	(update-panel-items))

(defun update-panel-items ()
	; sync inventory item frames and selector reticle to panel sprites
	(when (= (length *panel_item_sprites*) 8)
		(each (lambda (sp)
			(defq item (elem-get *item_inventory* (!)))
			(if (> item 0)
				(progn
					(. sp :sp_set_frame item)
					(def sp :sp_canvas_l *img_frm_16x16* :zoom *zoom*))
				(. sp :sp_set_frame -1)))
			*panel_item_sprites*)
		(when *panel_sight_sprite*
			(def *panel_sight_sprite* :sp_canvas_l *img_frm_16x16* :zoom *zoom*)
			(. *panel_sight_sprite* :sp_set_pos (* *item_selected* 16) 0))))

(defun item-select ()
	; cycle selected inventory slot left or right
	(cond
		((bits? *game_controls* +fkey_keyc)
			(unless *item_select_pressed*
				(setq *item_select_pressed* :t
					*item_selected* (% (+ *item_selected* 7) 8))
				(update-panel-items)))
		((bits? *game_controls* +fkey_keyd)
			(unless *item_select_pressed*
				(setq *item_select_pressed* :t
					*item_selected* (% (+ *item_selected* 1) 8))
				(update-panel-items)))
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

(defun update-panel-layers ()
	; tick all active sprites across panel layers
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