;;;;;;;;;;;;;;;;;;;;;;;;
; fanatic entity sprites
;;;;;;;;;;;;;;;;;;;;;;;;

(defq *player_man* :nil *game_controls* 0
	+man_comp_at 0
	; 11-step jump trajectory: 2 crouch wind-up frames, 8 rising frames, 1 apex frame
	+jump_offsets
	''((29 0) (29 0) (8 7) (8 6) (8 5) (8 4) (8 3) (8 3) (8 2) (8 1) (1 1)))

(defun step-walk-cycle (this start_frame end_frame)
	(defq tick (+ (get :man_walk_tick this) 1)
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
	(bind '(x y) (. this :sp_get_pos))
	(defq xv (get :man_xv this) yv (get :man_yv this)
		man_jump (get :man_jump this) h (get :sp_h this) w (get :sp_w this)
		tile_h +tile_height tile_w +tile_width
		max_x (- (* +map_width tile_w) w))

	(unless (bits? *game_controls* +fkey_keya)
		(setq *test_addon_pressed* :nil))

	(cond
		; active jump trajectory (committed arc, no item use)
		(man_jump
			(bind '(jf jv) (elem-get +jump_offsets man_jump))
			(. this :sp_set_frame jf)
			(when (/= jv 0)
				(setq x (+ x xv) y (- y jv)))
			(setq man_jump (inc man_jump))
			(if (= man_jump (length +jump_offsets))
				(def this :man_jump :nil)
				(def this :man_jump man_jump))
			(setq x (max 0 (min max_x x)))
			(. this :sp_set_pos x y)
			this)

		; normal ground & air movement
		(:t
			(defq y1 (+ h yv y)
				x_probe1 (+ x 8) x_probe2 (+ x 23)
				flags1 (. *layer_land* :get_tile_flags x_probe1 y1)
				flags2 (. *layer_land* :get_tile_flags x_probe2 y1))

			(ifn (or (bits? flags1 +fmap_climb +fmap_stand)
					 (bits? flags2 +fmap_climb +fmap_stand))
				(progn
					; falling in air: cancel any active attack animation
					(when (bits? (get :sp_flags this) +fsp_action)
						(def this :sp_flags (logand (get :sp_flags this) (lognot +fsp_action)))
						(set man_at :table :nil :speed 0))
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
					; active weapon attack action animation
					((bits? (get :sp_flags this) +fsp_action)
						(ifn (get :table man_at)
							(progn
								(def this :sp_flags (logand (get :sp_flags this) (lognot +fsp_action)))
								(if (< (get :sp_frame this) +frm_fanatic_l_duck)
									(. this :sp_set_frame +frm_fanatic_l_stance)
									(. this :sp_set_frame +frm_fanatic_l_duck)))
							this))

					; up control: climb ladder or initiate jump
					((bits? *game_controls* +fkey_up)
						(defq climb_y (+ y h -1)
							f1 (. *layer_land* :get_tile_flags (+ x 4) climb_y)
							f2 (. *layer_land* :get_tile_flags (+ x 27) climb_y))
						(if (and (bits? f1 +fmap_climb) (bits? f2 +fmap_climb))
							(progn
								; climb ladder
								(setq y (- y 4) x (logand (+ x 4) (neg tile_w)) xv 0 yv 0)
								(. this :sp_set_frame (+ +frm_fanatic_l_climb (logand (>> y 2) 3)))
								(def this :man_xv 0 :man_yv 0 :man_walk_tick 0))
							(progn
								; initiate jump
								(def this :man_jump 1 :man_yv 0)
								(bind '(jf jv) (first +jump_offsets))
								(. this :sp_set_frame jf))))

					; use currently selected item on fire button (only when grounded on surface)
					((and (bits? *game_controls* +fkey_keya) (not *test_addon_pressed*))
						(setq *test_addon_pressed* :t)
						(when (= *game_mode* +mode_auto)
							(item-auto-select))
						(defq item (elem-get *item_inventory* *item_selected*))
						(when (and (= item 0) (= *game_mode* +mode_assist))
							(audio-play-rpc *sfx_out*)
							(item-auto-select)
							(setq item (elem-get *item_inventory* *item_selected*)))
						(ifn (> item 0)
							(unless (= *game_mode* +mode_assist)
								(audio-play-rpc *sfx_out*))
							(when (and (= h 16) (<= (+ +frm_16x16_shield 1) item (+ +frm_16x16_shield 3)))
								; only standing projectile weapons force the player to stand up
								(setq y (- y 16) h 32)
								(. this :sp_set_bounds x y 32 32))
							(cond
								((= item +frm_16x16_shield)
									(defq uses (dec (elem-get *item_usage* *item_selected*)))
									(if (<= uses 0)
										(progn
											(elem-set *item_inventory* *item_selected* 0)
											(elem-set *item_usage* *item_selected* 0))
										(elem-set *item_usage* *item_selected* uses))
									(use-mase this))
								((= item (+ +frm_16x16_shield 5))
									(defq uses (dec (elem-get *item_usage* *item_selected*)))
									(if (<= uses 0)
										(progn
											(elem-set *item_inventory* *item_selected* 0)
											(elem-set *item_usage* *item_selected* 0))
										(elem-set *item_usage* *item_selected* uses))
									(use-super-mase this))
								((= item (+ +frm_16x16_shield 1))
									(defq uses (dec (elem-get *item_usage* *item_selected*)))
									(if (<= uses 0)
										(progn
											(elem-set *item_inventory* *item_selected* 0)
											(elem-set *item_usage* *item_selected* 0))
										(elem-set *item_usage* *item_selected* uses))
									(use-big-crossbow this))
								((= item (+ +frm_16x16_shield 2))
									(defq uses (dec (elem-get *item_usage* *item_selected*)))
									(if (<= uses 0)
										(progn
											(elem-set *item_inventory* *item_selected* 0)
											(elem-set *item_usage* *item_selected* 0))
										(elem-set *item_usage* *item_selected* uses))
									(use-small-crossbow this))
								((= item (+ +frm_16x16_shield 3))
									(defq uses (dec (elem-get *item_usage* *item_selected*)))
									(if (<= uses 0)
										(progn
											(elem-set *item_inventory* *item_selected* 0)
											(elem-set *item_usage* *item_selected* 0))
										(elem-set *item_usage* *item_selected* uses))
									(use-naptha this))
								((= item (+ +frm_16x16_shield 4))
									(defq uses (dec (elem-get *item_usage* *item_selected*)))
									(if (<= uses 0)
										(progn
											(elem-set *item_inventory* *item_selected* 0)
											(elem-set *item_usage* *item_selected* 0))
										(elem-set *item_usage* *item_selected* uses))
									(use-helper this))
								((= item (+ +frm_16x16_shield 6))
									(defq uses (dec (elem-get *item_usage* *item_selected*)))
									(if (<= uses 0)
										(progn
											(elem-set *item_inventory* *item_selected* 0)
											(elem-set *item_usage* *item_selected* 0))
										(elem-set *item_usage* *item_selected* uses))
									(use-super-helper this))
								((= item +frm_16x16_gold)
									(elem-set *item_inventory* *item_selected* 0)
									(elem-set *item_usage* *item_selected* 0)
									(setq *man_score* (+ *man_score* 500))
									(audio-play-rpc *sfx_spell*))
								((= item +frm_16x16_spells)
									(elem-set *item_inventory* *item_selected* 0)
									(elem-set *item_usage* *item_selected* 0)
									(use-yellow-spell this))
								((= item (+ +frm_16x16_spells 1))
									(elem-set *item_inventory* *item_selected* 0)
									(elem-set *item_usage* *item_selected* 0)
									(use-black-spell this))
								((= item (+ +frm_16x16_spells 2))
									(elem-set *item_inventory* *item_selected* 0)
									(elem-set *item_usage* *item_selected* 0)
									(use-green-spell this))
								((= item (+ +frm_16x16_spells 3))
									(elem-set *item_inventory* *item_selected* 0)
									(elem-set *item_usage* *item_selected* 0)
									(use-white-spell this))
								((= item (+ +frm_16x16_spells 4))
									(elem-set *item_inventory* *item_selected* 0)
									(elem-set *item_usage* *item_selected* 0)
									(use-red-spell this))
								((= item (+ +frm_16x16_spells 5))
									(elem-set *item_inventory* *item_selected* 0)
									(elem-set *item_usage* *item_selected* 0)
									(use-blue-spell this))
								(:t
									(audio-play-rpc *sfx_out*)))))

					; down control: climb down ladder or duck
					((bits? *game_controls* +fkey_down)
						(defq climb_y (+ y h)
							f1 (. *layer_land* :get_tile_flags (+ x 4) climb_y)
							f2 (. *layer_land* :get_tile_flags (+ x 27) climb_y))
						(if (and (bits? f1 +fmap_climb) (bits? f2 +fmap_climb))
							(progn
								; climb down ladder
								(setq y (+ y 4) x (logand (+ x 4) (neg tile_w)) xv 0 yv 0)
								(. this :sp_set_frame (+ +frm_fanatic_l_climb (logand (>> y 2) 3)))
								(def this :man_xv 0 :man_yv 0 :man_walk_tick 0))
							(progn
								; duck on ground
								(setq xv 0 yv 0)
								(. this :sp_set_frame +frm_fanatic_l_duck)
								(def this :man_xv 0 :man_yv 0 :man_walk_tick 0))))

					; walking left
					((bits? *game_controls* +fkey_left)
						(setq xv -4 x (+ x xv) y (* (/ y tile_h) tile_h))
						(def this :sp_dir -1 :man_xv xv)
						(step-walk-cycle this +frm_fanatic_l_walk (dec +frm_fanatic_l_jump)))

					; walking right
					((bits? *game_controls* +fkey_right)
						(setq xv 4 x (+ x xv) y (* (/ y tile_h) tile_h))
						(def this :sp_dir 1 :man_xv xv)
						(step-walk-cycle this +frm_fanatic_l_walk (dec +frm_fanatic_l_jump)))

					; no movement
					(:t
						(setq xv 0)
						(def this :man_xv 0 :man_walk_tick 0)
						(when (or (bits? flags1 +fmap_stand) (bits? flags2 +fmap_stand))
							(. this :sp_set_frame +frm_fanatic_l_stance)))))

			(setq x (max 0 (min max_x x)))
			(. this :sp_set_pos x y)
			(when (or (<= *man_power* 0) (<= *man_strength* 0))
				(. this :sp_kill))
			this)))

(defun cp-manbits (this that)
	(when (defq mv (get :mv_comp this))
		(bind '(x y) (. this :sp_get_pos))
		(defq yv (get :vy mv)
			h (get :sp_h this)
			tile_y (logand (+ y yv h) -16)
			oy (get :last_tile_y this))
		(def this :last_tile_y tile_y)
		(when (and (/= tile_y oy) (> yv 0))
			(when (bits? (. *layer_land* :get_tile_flags (+ x 8) tile_y) +fmap_stand)
				(defq new_y (- tile_y h))
				(. this :sp_set_pos x new_y)
				(set mv :vy (neg yv))
				(spawn-fizz-fx (+ x 8) tile_y)
				(when (= (get :sp_frame this) (+ +frm_16x16_manbits 1))
					(audio-play-rpc *sfx_clash1*))
				(defq bounces (inc (or (get :bounce_count this) 0)))
				(def this :bounce_count bounces)
				(when (>= bounces 4)
					(. this :sp_kill))))))

(defclass ManBits (frame vx vy) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-fizz-fx)
	; (ManBits frame vx vy) -> bouncing body part
	(defq mv (MV vx vy 0 1 3 10))
	(def this :sp_frame frame :mv_comp mv :last_tile_y -1 :bounce_count 0)
	(.-> this
		(:sp_add_component (CP (const cp-manbits)))
		(:sp_add_component mv)
		(:sp_add_component (CP (const offscreen-nodeath-update)))
		(:sp_set_frame frame)))

(defun dt-kill-man (this)
	; player died: explosion fx and burst into 5 bouncing pieces
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq cx (+ x (/ w 2)) cy (+ y (/ h 2)))
	(defq exp (. (Explosion) :sp_set_pos (- cx 16) (- cy 16)))
	(. *layer_fx* :add_front exp)
	(audio-play-rpc *sfx_explode*)
	(each (lambda ((f vx vy))
		(defq bit (ManBits f vx vy))
		(. bit :sp_set_pos (- cx 8) (- cy 8))
		(. *layer_player* :add_front bit))
		`((,(+ +frm_16x16_manbits 1) 0 -10)
		  (,(+ +frm_16x16_manbits 4) 1 -8)
		  (,(+ +frm_16x16_manbits 2) -1 -8)
		  (,+frm_16x16_manbits 2 -6)
		  (,(+ +frm_16x16_manbits 3) -2 -6)))
	(setq *man_dead_timer* 80))

(defclass Fanatic () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic dt-kill-man)
	; (Fanatic) -> sprite
	(def this :sp_type +ftp_man :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0
		:man_jump :nil)
	(.-> this
		; at (index 0 / +man_comp_at): player weapon body animations
		(:sp_add_component (AT 0 :nil :nil))
		; cp (index 1): player input, movement, jumping, and falling physics
		(:sp_add_component (CP player-man-update))
		; cp (index 2): ducking height and y bounds adjustment component
		(:sp_add_component (CP cp-manduck))
		; cl1..cl6 (indices 3..8): collision checks
		(:sp_add_component (CL *layer_items* +ftp_item *man_collision_table*))
		(:sp_add_component (CL *layer_missiles* -1 *man_collision_table*))
		(:sp_add_component (CL *layer_enemies*
			(logior +ftp_tower +ftp_horse +ftp_boarrider +ftp_carpet +ftp_skeleton_horse)
			*man_collision_table*))
		(:sp_add_component (CL *layer_enemies*
			(logior +ftp_knight +ftp_skeleton +ftp_monk +ftp_skeleton_monk)
			*man_collision_table*))
		(:sp_add_component (CL *layer_enemies* +ftp_footman *man_collision_table*))
		(:sp_add_component (CL *layer_bans* -1 *man_collision_table*))
		(:sp_set_frame +frm_fanatic_l_stance)))