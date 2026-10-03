;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; game specific seeded pseudo-random
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *game_seed* 0)

(defun game-seed (seed)
	; (game-seed seed) -> set 32-bit unsigned game seed
	(setq *game_seed* (logand seed 0xffffffff)))

(defun game-random (range)
	; (game-random range) -> pseudo-random integer in 0..(range - 1)
	(if (<= range 1) 0
		(setq *game_seed* (logand (logxor (* *game_seed* 17) 0xa5a5a5a5) 0xffffffff))
		(>>> (* range (>>> *game_seed* 16)) 16)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; layer management helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	*item_inventory* (list +frm_16x16_shield 0 0 0 0 0 0 0)
	*item_usage* (list 48 0 0 0 0 0 0 0)
	*item_selected* 0 *game_mode* +mode_auto *game_frame_count* 0
	*item_select_pressed* :nil *item_drop_cnt* 0 *fire_held_first* :nil
	*panel_item_sprites* (list) *panel_sight_sprite* :nil
	+select_table
	; item priority table matching cpp selecttable (indexed by item - +frm_16x16_gold)
	''(19 9 11 9 12 16 14 17 15 10 18 13 20 20 8 8 8 8 8 8 8 8 8 8))

(defun item-auto-select ()
	; select best item from inventory based on priority table
	(defq idx 0 best_idx *item_selected* max_pri 0)
	(while (< idx (length *item_inventory*))
		(defq item (elem-get *item_inventory* idx))
		(when (> item 0)
			(defq pri (elem-get +select_table (- item +frm_16x16_gold)))
			(when (> pri max_pri)
				(setq max_pri pri best_idx idx)))
		(++ idx))
	(when (> max_pri 0)
		(setq *item_selected* best_idx))
	*item_selected*)

(defun item-select ()
	; move selector left/right based on *game_controls* matching cpp item_select
	(if (bits? *game_controls* +fkey_keyc +fkey_keyd)
		(unless (bits? *game_flags* +fflag_iselect)
			(setq *game_flags* (logior *game_flags* +fflag_iselect))
			(if (bits? *game_controls* +fkey_keyc)
				(setq *item_selected* (% (+ *item_selected* 7) 8))
				(setq *item_selected* (% (+ *item_selected* 1) 8))))
		(setq *game_flags* (logand *game_flags* (lognot +fflag_iselect)))))

(defun clear-layer (layer)
	; remove all child views from layer
	(each (# (. %0 :sub)) (. layer :children)))

(defun clear-sprite-layers ()
	; remove all active sprites from all 7 sprite layers, without death hooks,
	; and put the camera, which sounds pan about, back to the origin
	(setq *cam_x* 0 *cam_y* 0)
	(each (# (clear-layer %0)) +sprite_layers))

(defun set-world-layers-pos (x y)
	; position all children of *world_layers*
	(each (# (. %0 :set_pos x y)) +world_layers))

(defun set-world-layers-size (w h)
	; size all children of *world_layers*
	(each (# (def %0 :zoom *zoom*) (. %0 :set_size w h)) +world_layers))

(defun update-sprite-canvas (sp)
	(case (get :sp_canvas_tag sp)
		(:fanatic
			(set sp :sp_canvas_l *img_fanatic_l* :sp_canvas_r *img_fanatic_r*))
		(:32c32
			(set sp :sp_canvas_l *img_frm_32c32*))
		(:16x16
			(set sp :sp_canvas_l *img_frm_16x16*))
		(:16x16_lr
			(set sp :sp_canvas_l *img_frm_16x16_l* :sp_canvas_r *img_frm_16x16_r*))
		(:16x32
			(set sp :sp_canvas_l *img_frm_16x32*))
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
			(set sp :sp_canvas_l *img_frm_64x64*))))

(defun rescale-active-sprites ()
	; rescale positions, dimensions and canvases of all live sprites
	(each (lambda (layer)
		(each (lambda (sp)
			(when (Sprite? sp)
				(def sp :zoom *zoom*)
				(bind '(x y w h) (. sp :sp_get_bounds))
				(. sp :sp_set_bounds x y w h)
				(update-sprite-canvas sp)))
			(. layer :children)))
		(cat +sprite_layers +panel_layers)))

(defun update-sprite-layers ()
	; tick all active sprites across world layers
	(each (# (each (lambda (sp) (when (Sprite? sp) (. sp :sp_update))) (. %0 :children)))
		+sprite_layers))

(defun check-empty-mase ()
	; if player has no offensive weapons or attack spells, grant 6 mace swings in first free slot
	(when (= (logand (++ *game_frame_count*) 127) 0)
		(unless (some (# (< 0 %0 +frm_16x16_spells3)) *item_inventory*)
			(when (defq empty_idx (find 0 *item_inventory*))
				(elem-set *item_inventory* empty_idx +frm_16x16_shield)
				(elem-set *item_usage* empty_idx 6)
				(when (= *game_mode* +mode_auto)
					(item-auto-select))))))

(defun get-xy-outer (w h)
	; pick a random start point just outside the playable area
	(case (game-random 4)
		(0 (list (- (game-random (+ +window_width (* 2 w))) w) (neg h)))
		(1 (list (- (game-random (+ +window_width (* 2 w))) w) +window_height))
		(2 (list (neg w) (- (game-random (+ +window_height (* 2 h))) h)))
		(3 (list +window_width (- (game-random (+ +window_height (* 2 h))) h)))))

(defq *cam_x* 0 *cam_y* 0)

;how far a sound at the screen edge pans, 255 is hard left or right. as
;most of the action is on the right, keep some of it in the other ear
(defq +pan_spread 170)

(defun play-sfx (sfx &optional sp)
	; (play-sfx sfx [sprite])
	;play a sound effect, panned to where the sprite is on the screen
	(audio-play-rpc sfx (if sp
		(max (const (neg +pan_spread)) (min +pan_spread
			(/ (* (- (+ (get :sp_x sp) (/ (get :sp_w sp) 2)) *cam_x*
				(const (/ +window_width 2))) +pan_spread) (const (/ +window_width 2)))))
		0)))

(defun update-camera (sprite)
	(bind '(x y w h) (. sprite :sp_get_bounds))
	(defq cam_x (- (+ x (/ w 2)) (const (/ +window_width 2)))
		cam_y (- (+ y h -16) (const (/ +window_height 2)))
		max_cam_x (- (* +map_width +tile_width) +window_width)
		max_cam_y (- (* +map_height +tile_height) +window_height)
		cx (max 0 (min max_cam_x cam_x))
		cy (max 0 (min max_cam_y cam_y)))
	; store pure unscaled simulation coordinates
	(setq *cam_x* cx *cam_y* cy)
	; scroll GUI rendering layers
	(set-world-layers-pos (* *zoom* (neg cx)) (* *zoom* (neg cy))))

(defun update-panel-status ()
	; update enemy stack flags and power/strength level indicators directly
	(defq stack_val (max 0 (min 32 (* (length *enemy_stack*) 2)))
		flag_y (+ +status_flag_y (- 32 stack_val))
		flag_oy (- 32 stack_val)
		power_val (max 0 (min 32 (/ (* (max 0 (min +max_power *man_power*)) 32) +max_power)))
		power_y (+ +status_level_y (- 32 power_val))
		power_oy (- 32 power_val)
		strength_val (max 0 (min 32 (/ (* (max 0 (min +max_strength *man_strength*)) 32) +max_strength)))
		strength_y (+ +status_level_y (- 32 strength_val))
		strength_oy (- 32 strength_val))
	(def *layer_panel_items* :canvas *img_frm_16x16*
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
	(update-panel-status)
	(each (# (each (lambda (sp) (when (Sprite? sp) (. sp :sp_update))) (. %0 :children)))
		+panel_layers))

(defun update-frame ()
	; common screen and layer updates across all game states
	(item-select)
	(update-sprite-layers)
	(update-panel-layers)
	(. *world_scroll* :dirty_all)
	(. *panel_layers* :dirty_all))
