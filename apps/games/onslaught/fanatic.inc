;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/fanatic.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
; fanatic entity sprites
;;;;;;;;;;;;;;;;;;;;;;;;

(defq *player_man* :nil *game_controls* 0
	+man_comp_at 0)

(defun step-walk-cycle (this start_frame end_frame)
	(defq tick (+ (or (get :man_walk_tick this) 0) 1)
		cycle_len (+ (- end_frame start_frame) 1)
		frame_idx (+ start_frame (% tick cycle_len)))
	(def this :man_walk_tick tick)
	(. this :sp_set_frame frame_idx))

(defun cp-manduck (this that)
	; adjust sprite height and y when ducking (frame >= +frm_fanatic_l_duck)
	(defq h (get :sp_h this)
		f (get :sp_frame this))
	(if (< f +frm_fanatic_l_duck)
		(when (/= h 32)
			(. this :sp_set_bounds (get :sp_x this) (- (get :sp_y this) 16) 32 32))
		(when (= h 32)
			(. this :sp_set_bounds (get :sp_x this) (+ (get :sp_y this) 16) 32 16)))
	this)

(defun player-man-update (this that)
	; this = sprite, that = component instance
	(defq man_at (elem-get (get :sp_components this) +man_comp_at))
	(cond
		; handle active weapon action animation completion
		((get :action_playing this)
			(ifn (get :table man_at)
				(progn
					(def this :action_playing :nil)
					(if (< (get :sp_frame this) +frm_fanatic_l_duck)
						(. this :sp_set_frame +frm_fanatic_l_stance)
						(. this :sp_set_frame +frm_fanatic_l_duck)))
				this))

		; test cycle through all 5 weapon actions on fire button
		((and (bits? *game_controls* +fkey_keya) (not *test_addon_pressed*))
			(setq *test_addon_pressed* :t)
			(trigger-player-addon this *test_addon_idx*)
			(setq *test_addon_idx* (% (+ *test_addon_idx* 1) 5))
			this)

		(:t
			(unless (bits? *game_controls* +fkey_keya)
				(setq *test_addon_pressed* :nil))

			(bind '(x y) (. this :sp_get_pos))
			(defq xv (or (get :man_xv this) 0) yv (or (get :man_yv this) 0)
				h (get :sp_h this) w (get :sp_w this) walk_spd 4
				tile_h +tile_height tile_w +tile_width y1 (+ h yv y)
				x_probe1 (+ x 8) x_probe2 (+ x 23)
				flags1 (. *layer_land* :get_tile_flags x_probe1 y1)
				flags2 (. *layer_land* :get_tile_flags x_probe2 y1))

			(ifn (or (bits? flags1 +fmap_climb +fmap_stand)
					 (bits? flags2 +fmap_climb +fmap_stand))
				(progn
					; falling in air
					(. this :sp_set_frame +frm_fanatic_l_fall)
					(setq yv (min 10 (inc yv)) x (+ x xv) y (+ y yv))
					(if (= yv 10) (setq xv 0))
					(def this :man_xv xv :man_yv yv))
				; on surface (floor or ladder)
				(when (/= yv 0)
					(if (= yv 10)
						(. this :sp_set_frame +frm_fanatic_l_duck)
						(. this :sp_set_frame +frm_fanatic_l_stance))
					(setq y (- (* (/ y1 tile_h) tile_h) h) yv 0 xv 0)
					(def this :man_yv 0 :man_xv 0))
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
							(setq y (- y walk_spd) x ladder_x xv 0 yv 0)
							(. this :sp_set_frame (+ +frm_fanatic_l_climb (logand (/ y walk_spd) 3)))
							(def this :man_xv 0 :man_yv 0 :man_walk_tick 0)))

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
								(setq y (+ y walk_spd) x ladder_x xv 0 yv 0)
								(. this :sp_set_frame (+ +frm_fanatic_l_climb (logand (/ y walk_spd) 3)))
								(def this :man_xv 0 :man_yv 0 :man_walk_tick 0))
							; duck on ground when cannot climb down
							(setq xv 0 yv 0)
							(. this :sp_set_frame +frm_fanatic_l_duck)
							(def this :man_xv 0 :man_yv 0 :man_walk_tick 0)))

					; walking left
					((bits? *game_controls* +fkey_left)
						(setq xv (neg walk_spd) x (+ x xv) y (* (/ y tile_h) tile_h))
						(def this :sp_dir -1 :man_xv xv)
						(step-walk-cycle this +frm_fanatic_l_walk (dec +frm_fanatic_l_jump)))

					; walking right
					((bits? *game_controls* +fkey_right)
						(setq xv walk_spd x (+ x xv) y (* (/ y tile_h) tile_h))
						(def this :sp_dir 1 :man_xv xv)
						(step-walk-cycle this +frm_fanatic_l_walk (dec +frm_fanatic_l_jump)))

					; no movement
					(:t
						(setq xv 0)
						(def this :man_xv 0 :man_walk_tick 0)
						(when (or (bits? flags1 +fmap_stand) (bits? flags2 +fmap_stand))
							(. this :sp_set_frame +frm_fanatic_l_stance)))))

			; save new position clamped to map bounds
			(defq max_x (- (* +map_width tile_w) w))
			(setq x (max 0 (min max_x x)))
			(. this :sp_set_pos x y)
			this)))

(defclass Fanatic () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; (Fanatic) -> sprite
	(def this :sp_type +ftp_man :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0
		:action_playing :nil)
	(.-> this
		; at (index 0 / +man_comp_at): player weapon body animations
		(:sp_add_component (AT 0 :nil :nil))
		; player input, movement, jumping, and falling physics (index 1)
		(:sp_add_component (CP (const player-man-update)))
		; ducking height and y bounds adjustment component (index 2)
		(:sp_add_component (CP (const cp-manduck)))
		; collision checks in sequence (indices 3..7)
		(:sp_add_component (CL *layer_missiles* -1 *man_collision_table*))
		(:sp_add_component (CL *layer_enemies*
			(logior +ftp_tower +ftp_horse +ftp_boarrider +ftp_carpet +ftp_skeleton_horse)
			*man_collision_table*))
		(:sp_add_component (CL *layer_enemies*
			(logior +ftp_knight +ftp_skeleton +ftp_monk)
			*man_collision_table*))
		(:sp_add_component (CL *layer_enemies* +ftp_footman *man_collision_table*))
		(:sp_add_component (CL *layer_bans* -1 *man_collision_table*))
		(:sp_set_frame +frm_fanatic_l_stance)))