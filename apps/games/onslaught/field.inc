;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught field battle sequence
; handles field map loop, QAOP controls, player man physics & camera tracking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	*player_man* :nil
	*game_controls* 0
	*current_map_idx* 1)

(defun step-walk-cycle (man start_frame end_frame)
	(defq tick (+ (or (get :man_walk_tick man) 0) 1)
		cycle_len (+ (- end_frame start_frame) 1)
		frame_idx (+ start_frame (% tick cycle_len)))
	(def man :man_walk_tick tick)
	(. man :set_frame frame_idx))

(defun update-camera (man_x man_y)
	; center camera on player man
	(defq cam_x (- (+ man_x 16) (const (/ +window_width 2)))
		cam_y (- (+ man_y 16) (const (/ +window_height 2)))
		max_cam_x (- (* +map_width +tile_width) +window_width)
		max_cam_y (- (* +map_height +tile_height) +window_height))
	; scroll all world layers by negative camera offset
	(set-world-layers-pos
		(* *zoom* (neg (max 0 (min max_cam_x cam_x))))
		(* *zoom* (neg (max 0 (min max_cam_y cam_y))))))

(defun player-man-update (man)
	; physics & qaop controls for player man (fanatic 32x32)
	(bind '(x y) (. man :sp_get_pos))
	(defq xv (or (get :man_xv man) 0) yv (or (get :man_yv man) 0)
		h 32 w 32 walk_spd 4
		tile_h +tile_height tile_w +tile_width
		y1 (+ h yv y)
		x_probe1 (+ x 8) x_probe2 (+ x 23)
		flags1 (. *layer_land* :get_tile_flags x_probe1 y1)
		flags2 (. *layer_land* :get_tile_flags x_probe2 y1))

	(ifn (or (bits? flags1 +fmap_climb +fmap_stand)
			 (bits? flags2 +fmap_climb +fmap_stand))
		(progn
			; falling in air
			(. man :set_frame +frm_fanatic_l_fall)
			(setq yv (min 10 (inc yv)) x (+ x xv) y (+ y yv))
			(if (= yv 10) (setq xv 0))
			(def man :man_xv xv :man_yv yv))
		; on surface (floor or ladder)
		(when (/= yv 0)
			(if (= yv 10)
				(. man :set_frame +frm_fanatic_l_duck)
				(. man :set_frame +frm_fanatic_l_stance))
			(setq y (- (* (/ y1 tile_h) tile_h) h) yv 0 xv 0)
			(def man :man_yv 0 :man_xv 0))
		(cond
			; climbing up
			((bits? *game_controls* +fkey_up)
				(defq climb_y (+ y h -1)
					  f1 (. *layer_land* :get_tile_flags x_probe1 climb_y)
					  f2 (. *layer_land* :get_tile_flags x_probe2 climb_y))
				(when (and (bits? f1 +fmap_climb) (bits? f2 +fmap_climb))
					(defq tx1 (/ x_probe1 tile_w) tx2 (/ x_probe2 tile_w)
						  ladder_tx (if (and (= tx1 tx2)
											 (bits? (. *layer_land* :get_tile_flags (- (* tx1 tile_w) 1) climb_y) +fmap_climb))
										(dec tx1)
										tx1)
						  ladder_x (* ladder_tx tile_w))
					(setq y (- y walk_spd)
						  x ladder_x
						  xv 0 yv 0)
					(. man :set_frame (+ +frm_fanatic_l_climb (logand (/ y walk_spd) 3)))
					(def man :man_xv 0 :man_yv 0 :man_walk_tick 0)))

			; climbing down or ducking
			((bits? *game_controls* +fkey_down)
				(defq climb_y (+ y h)
					  f1 (. *layer_land* :get_tile_flags x_probe1 climb_y)
					  f2 (. *layer_land* :get_tile_flags x_probe2 climb_y))
				(if (and (bits? f1 +fmap_climb) (bits? f2 +fmap_climb))
					(progn
						(defq tx1 (/ x_probe1 tile_w) tx2 (/ x_probe2 tile_w)
							  ladder_tx (if (and (= tx1 tx2)
												 (bits? (. *layer_land* :get_tile_flags (- (* tx1 tile_w) 1) climb_y) +fmap_climb))
											(dec tx1)
											tx1)
							  ladder_x (* ladder_tx tile_w))
						(setq y (+ y walk_spd)
							  x ladder_x
							  xv 0 yv 0)
						(. man :set_frame (+ +frm_fanatic_l_climb (logand (/ y walk_spd) 3)))
						(def man :man_xv 0 :man_yv 0 :man_walk_tick 0))
					; duck on ground when cannot climb down
					(setq xv 0 yv 0)
					(. man :set_frame +frm_fanatic_l_duck)
					(def man :man_xv 0 :man_yv 0 :man_walk_tick 0)))

			; walking left
			((bits? *game_controls* +fkey_left)
				(setq xv (neg walk_spd)
					  x (+ x xv)
					  y (* (/ y tile_h) tile_h))
				(def man :sp_dir -1 :man_xv xv)
				(step-walk-cycle man +frm_fanatic_l_walk (dec +frm_fanatic_l_jump)))

			; walking right
			((bits? *game_controls* +fkey_right)
				(setq xv walk_spd
					  x (+ x xv)
					  y (* (/ y tile_h) tile_h))
				(def man :sp_dir 1 :man_xv xv)
				(step-walk-cycle man +frm_fanatic_l_walk (dec +frm_fanatic_l_jump)))

			; no movement
			(:t
				(setq xv 0)
				(def man :man_xv 0 :man_walk_tick 0)
				(when (or (bits? flags1 +fmap_stand) (bits? flags2 +fmap_stand))
					(. man :set_frame +frm_fanatic_l_stance)))))

	; save new position clamped to map bounds
	(defq max_x (- (* +map_width tile_w) w))
	(setq x (max 0 (min max_x x)))
	(. man :sp_set_pos x y)
	(list x y))

(defun field-sequence-start (&optional layer map_idx)
	; switch to field battle scene
	(setd map_idx *current_map_idx*)
	(reset-layers-field map_idx)
	; spawn player man at starting position (x=8, y=208)
	(defq man (ManSprite))
	(def man :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0)
	(. man :sp_set_pos 8 208)
	(. *layer_player* :add_front man)
	(setq *player_man* man)
	; position camera on player
	(update-camera 8 208)
	man)

(defun field-sequence-update ()
	; tick field map state
	(bind '(mx my) (player-man-update *player_man*))
	(update-camera mx my)
	; update all active sprites across all 7 layers
	(update-sprite-layers))
