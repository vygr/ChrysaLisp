;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught battle state
;;;;;;;;;;;;;;;;;;;;;;;;

(defq +max_enemy_stack 16 *enemy_max* 6 *mine_count* 0 *mine_max* 3
	*current_map_idx* 1 *game_level* +game_level_field *battle_state_count* 0
	*enemy_army* 0 *enemy_count* 0 *enemy_spawn_delay* 1 *enemy_wizshot* 0
	*enemy_stack* (list) *enemy_banner_captured* :nil
	*man_banner* 1 *enemy_banner* 5 *campaign_active* :nil
	+army_names
	''("HILLMEN" "NECROMANTIC" "ROBBER" "BOARRIDER" "MERCENARY"
	   "KNIGHTLY" "BERSERKER" "MYSTIC" "BALISTIC" "POWDER"
	   "CAULDRON" "MONASTIC" "JUGGERNAUT" "PLAGUE")
	+army_tables
	`'(; 0: hillmen
	   (,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_beserk
	    ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_beserk
	    ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_beserk)
	   ; 1: necromantic
	   (,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_carpet
	    ,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_carpet
	    ,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_wizard ,+ftp_carpet)
	   ; 2: robber
	   (,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_cannon
	    ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_cannon
	    ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman)
	   ; 3: boarrider
	   (,+ftp_boarrider ,+ftp_boarrider ,+ftp_boarrider ,+ftp_boarrider ,+ftp_boarrider ,+ftp_monk
	    ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_monk
	    ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_monk)
	   ; 4: mercenary
	   (,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_tower
	    ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_tower ,+ftp_knight
	    ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_spearman ,+ftp_knight)
	   ; 5: knightly
	   (,+ftp_horse ,+ftp_horse ,+ftp_horse ,+ftp_horse ,+ftp_horse ,+ftp_boarrider
	    ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_spearman
	    ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_knight ,+ftp_spearman)
	   ; 6: berserker
	   (,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_spearman
	    ,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_spearman
	    ,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_beserk ,+ftp_spearman)
	   ; 7: mystic
	   (,+ftp_carpet ,+ftp_carpet ,+ftp_carpet ,+ftp_carpet ,+ftp_carpet ,+ftp_tower
	    ,+ftp_carpet ,+ftp_carpet ,+ftp_carpet ,+ftp_carpet ,+ftp_tower ,+ftp_carpet
	    ,+ftp_carpet ,+ftp_carpet ,+ftp_carpet ,+ftp_carpet ,+ftp_carpet ,+ftp_spearman)
	   ; 8: balistic
	   (,+ftp_balista ,+ftp_balista ,+ftp_balista ,+ftp_balista ,+ftp_cannon ,+ftp_balista
	    ,+ftp_balista ,+ftp_balista ,+ftp_balista ,+ftp_balista ,+ftp_balista ,+ftp_cannon
	    ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman)
	   ; 9: powder
	   (,+ftp_cannon ,+ftp_cannon ,+ftp_cannon ,+ftp_cannon ,+ftp_cannon ,+ftp_footman
	    ,+ftp_cannon ,+ftp_cannon ,+ftp_cannon ,+ftp_cannon ,+ftp_cannon ,+ftp_footman
	    ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman)
	   ; 10: cauldron
	   (,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_horse
	    ,+ftp_oil ,+ftp_oil ,+ftp_oil ,+ftp_oil ,+ftp_oil ,+ftp_knight
	    ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_footman ,+ftp_knight)
	   ; 11: monastic
	   (,+ftp_monk ,+ftp_monk ,+ftp_monk ,+ftp_monk ,+ftp_monk ,+ftp_beserk
	    ,+ftp_monk ,+ftp_monk ,+ftp_monk ,+ftp_monk ,+ftp_beserk ,+ftp_monk
	    ,+ftp_monk ,+ftp_monk ,+ftp_monk ,+ftp_monk ,+ftp_beserk ,+ftp_monk)
	   ; 12: juggernaut
	   (,+ftp_tower ,+ftp_tower ,+ftp_tower ,+ftp_tower ,+ftp_tower ,+ftp_balista
	    ,+ftp_tower ,+ftp_tower ,+ftp_tower ,+ftp_tower ,+ftp_tower ,+ftp_oil
	    ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_spearman ,+ftp_balista)
	   ; 13: plague
	   (,+ftp_skeleton_horse ,+ftp_skeleton_horse ,+ftp_skeleton_horse ,+ftp_skeleton_monk ,+ftp_skeleton_monk ,+ftp_skeleton_monk
	    ,+ftp_skeleton ,+ftp_skeleton ,+ftp_skeleton_horse ,+ftp_skeleton_monk ,+ftp_skeleton_monk ,+ftp_skeleton_monk
	    ,+ftp_skeleton ,+ftp_skeleton ,+ftp_skeleton_horse ,+ftp_skeleton_monk ,+ftp_skeleton_monk ,+ftp_skeleton_monk)))

(defun is-stationary-type (type)
	; balista, cannon, oil, beserk
	(bits? type +ftp_balista +ftp_cannon +ftp_oil +ftp_beserk))

;;;;;;;;;;;;;;;;;;;;;;
; enemy spawn routines
;;;;;;;;;;;;;;;;;;;;;;

(defun getxy (w h dir)
	; C++ getxy: find solid ground surface at visible screen edge for sprite of size (w, h)
	(bind '(lx ly &ignore) (. *world_scroll* :get_relative *layer_land*))
	(defq cam_x (/ (neg lx) *zoom*)
		cam_y (/ (neg ly) *zoom*)
		w1 (- w 1)
		x (if (= dir 1)
			(+ cam_x (- +window_width 1))
			(- cam_x w1))
		x1 (if (= dir 1) (+ x w1) cam_x)
		rand_y (game-random (* +tile_height (- (/ +window_height +tile_height) 1)))
		y (logand (+ rand_y cam_y) (neg +tile_height))
		map_w_px (* +tile_width +map_width)
		map_h_px (* +tile_height +map_height)
		found :nil)
	(while (and (not found) (< y map_h_px))
		(defq f1 (if (< x 0) +fmap_stand (. *layer_land* :get_tile_flags x y))
			f2 (if (>= x1 map_w_px) +fmap_stand (. *layer_land* :get_tile_flags x1 y)))
		(if (and (bits? f1 +fmap_stand) (bits? f2 +fmap_stand)
				 (not (bits? f1 +fmap_climb)) (not (bits? f2 +fmap_climb)))
			(setq found :t)
			(setq y (+ y +tile_height))))
	(when found
		(defq sy (- y h))
		(if (and (>= sy cam_y) (< sy (+ cam_y +window_height)))
			(list x sy)
			:nil)))

(defun spawn-enemy-right ()
	; spawn an enemy on the right side of the screen moving left
	(defq army_tbl (elem-get +army_tables *enemy_army*)
		lvl_offset (* *game_level* 6)
		unit_idx (+ (game-random 6) lvl_offset)
		type (elem-get army_tbl unit_idx))
	(bind '(w h) (enemy-size type))
	(when (defq pt (getxy w h 1))
		(bind '(x y) pt)
		(defq stationary (is-stationary-type type))
		(when (or (not stationary)
				  (and (< x (const (- (* +map_width +tile_width) 32)))
					   (not (sprite-collide *layer_enemies* x y w h
							(logior +ftp_balista +ftp_cannon +ftp_oil +ftp_beserk)))))
			(defq sp (create-enemy-type type)
				mv (elem-get (get :sp_components sp) +en_comp_mv))
			(. sp :sp_set_pos x y)
			(def sp :sp_dir -1)
			(when mv
				(set mv :vx (neg (get :max_vx mv))))
			(. *layer_enemies* :add_front sp)
			(++ *enemy_count*))))

(defun spawn-enemy-left ()
	; spawn an overrun enemy from the stack on the left side moving right
	(if (empty? *enemy_stack*) (spawn-enemy-right)
		(defq type (last *enemy_stack*))
		(bind '(w h) (enemy-size type))
		(ifn (defq pt (getxy w h -1)) (spawn-enemy-right)
			(bind '(x y) pt)
			(defq stationary (is-stationary-type type))
			(if (and stationary
					 (or (<= x 0)
						 (sprite-collide *layer_enemies* x y w h
							(logior +ftp_balista +ftp_cannon +ftp_oil +ftp_beserk))))
				(spawn-enemy-right)
				(pop *enemy_stack*)
				(defq sp (create-enemy-type type)
					mv (elem-get (get :sp_components sp) +en_comp_mv))
				(. sp :sp_set_pos x y)
				(def sp :sp_dir 1)
				(when mv
					(set mv :vx (get :max_vx mv)))
				(. *layer_enemies* :add_front sp)
				(++ *enemy_count*)))))

(defun init-enemies ()
	; spawn enemies according to current faction table and delay
	(when (< *enemy_count* *enemy_max*)
		(-- *enemy_spawn_delay*)
		(when (<= *enemy_spawn_delay* 0)
			(setq *enemy_spawn_delay* (+ (game-random 8) 8))
			(when *player_man*
				(defq pdir (get :sp_dir *player_man*))
				(if (or (= pdir -1) *enemy_banner_captured*)
					(if (> (length *enemy_stack*) 0)
						(spawn-enemy-left)
						(unless *enemy_banner_captured*
							(spawn-enemy-right)))
					(spawn-enemy-right))))))

(defun init-mines ()
	; spawn ground mines ahead of player in *layer_bans*
	(when (and (/= *game_level* +game_level_defend)
			   (< *mine_count* *mine_max*)
			   *player_man*)
		(defq dir (get :sp_dir *player_man*))
		(when (defq pt (getxy 16 8 dir))
			(bind '(x y) pt)
			(when (and (>= x 0) (< x (const (- (* +map_width +tile_width) 16))))
				(unless (sprite-collide *layer_bans* (- x 28) y 72 8 +ftp_enemy_fire)
					(defq mine (Mine))
					(. mine :sp_set_pos x y)
					(. *layer_bans* :add_front mine)
					(++ *mine_count*))))))

;;;;;;;;;;;;;;;;;;;;
; battle state hooks
;;;;;;;;;;;;;;;;;;;;

(defun battle-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
	(. *layer_sky* :sky_random)
	(case *game_level*
		(+game_level_seige
			(. *layer_land* :load_seige_map))
		(+game_level_defend
			(. *layer_land* :load_defend_map))
		(:t
			(setq *game_level* +game_level_field
				  *current_map_idx* 1)
			(. *layer_land* :load_field_map 1)))
	(def (defq man (Fanatic)) :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0)
	(. man :sp_set_pos 8 208)
	(. *layer_player* :add_front man)
	(setq *player_man* man
		  *man_power* +max_power
		  *man_strength* +max_strength
		  *enemy_stack* (list)
		  *enemy_banner_captured* :nil
		  *enemy_count* 0
		  *enemy_spawn_delay* 1
		  *mine_count* 0
		  *mine_max* (+ (game-random 3) 1)
		  *enemy_max* (if (= *menu_difficulty* 0) (+ (game-random 2) 2) (+ (game-random 4) 2)))
	(unless *campaign_active*
		(setq *campaign_active* :t
			  *enemy_army* (game-random (length +army_tables))
			  *enemy_wizshot* (game-random 2)
			  *man_banner* (+ +frm_16x16_banners (game-random 16))
			  *enemy_banner* (+ +frm_16x16_banners (game-random 16)))
		(when (= *man_banner* *enemy_banner*)
			(setq *enemy_banner* (+ +frm_16x16_banners (% (+ (- *enemy_banner* +frm_16x16_banners) 1) 16)))))
	; spawn banners
	(bind '(mx my ex ey) (case *game_level*
		(+game_level_seige '(16 192 2016 0))
		(+game_level_defend '(16 0 2016 192))
		(:t '(16 128 2016 128))))
	(defq man_ban (Banner +ftp_man_banner *man_banner*))
	(. man_ban :sp_set_pos mx my)
	(. *layer_bans* :add_front man_ban)
	(defq enemy_ban (Banner +ftp_enemy_banner *enemy_banner*))
	(. enemy_ban :sp_set_pos ex ey)
	(. *layer_bans* :add_front enemy_ban)
	(record-battle-start)
	(update-camera *player_man*))

(defun battle-state-update ()
	; check for player death sequence, battle lost, or battle won
	(cond
		((empty? (. *layer_player* :children))
			; death sequence finished (all manbits left the screen)
			(record-battle-finish)
			(setq *campaign_active* :nil)
			(goto-title +game_state_hiscore))
		((and *player_man* (get :sp_dead *player_man*))
			; man is dying, manbits are currently bouncing/flying off the screen
			:nil)
		((>= (length *enemy_stack*) +max_enemy_stack)
			(record-battle-finish)
			(setq *campaign_active* :nil)
			(setq *game_state* +game_state_battle_lost))
		(*enemy_banner_captured*
			(record-battle-finish)
			(setq *game_state* +game_state_battle_won))
		(:t
			(record-battle-tick *game_controls*)
			(init-mines)
			(init-enemies)
			(update-camera *player_man*))))

(defun cp-manstance (this that)
	(bind '(x y) (. this :sp_get_pos))
	(defq yv (get :man_yv this)
		h (get :sp_h this)
		y1 (+ h yv y)
		f1 (. *layer_land* :get_tile_flags (+ x 8) y1)
		f2 (. *layer_land* :get_tile_flags (+ x 23) y1))
	(if (and (not (bits? f1 (logior +fmap_climb +fmap_stand)))
			 (not (bits? f2 (logior +fmap_climb +fmap_stand))))
		(progn
			; falling
			(. this :sp_set_frame +frm_fanatic_l_fall)
			(setq yv (min 10 (inc yv)))
			(setq y (+ y yv)))
		(progn
			; on floor: stance animation continues
			(when (/= yv 0)
				(setq y (- (logand y1 -16) 32))
				(setq yv 0))))
	(def this :man_yv yv)
	(. this :sp_set_pos x y))

(defq +at_man_stance
	`'(,+frm_fanatic_l_stance ,+frm_fanatic_l_stance1))

(defclass ManStance (x y dir) (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; player victory dance sprite
	(def this :sp_dir dir :man_yv 0)
	(. this :sp_set_pos x y)
	(.-> this
		(:sp_add_component (AT 3 +at_man_stance))
		(:sp_add_component (CP (const cp-manstance)))
		(:sp_set_frame +frm_fanatic_l_stance)))

(defun battle-won-state-init ()
	(setq *battle_state_count* 0)
	; replace player with dancing victory stance without death explosion
	(when *player_man*
		(bind '(px py) (. *player_man* :sp_get_pos))
		(defq pdir (get :sp_dir *player_man*))
		(def *player_man* :sp_death :nil)
		(. *player_man* :sp_kill)
		(setq *player_man* (ManStance px py pdir))
		(. *layer_player* :add_front *player_man*))
	(clear-layer *layer_enemies*)
	(clear-layer *layer_missiles*)
	(setq *enemy_stack* (list)))

(defun battle-won-state-update ()
	; tick victory dance animation and track camera
	(if *player_man* (update-camera *player_man*))
	(++ *battle_state_count*)
	(when (>= *battle_state_count* 96)
		(when (= *game_level* +game_level_seige)
			(setq *campaign_active* :nil))
		(defq next_level (case *game_level*
				(+game_level_field +game_level_seige)
				(+game_level_seige +game_level_field)
				(:t +game_level_field)))
		(setq *game_level* next_level)
		(goto-title +game_state_battle)))

(defun battle-lost-state-init ()
	(setq *battle_state_count* 0)
	; replace player with stance without death explosion
	(when *player_man*
		(bind '(px py) (. *player_man* :sp_get_pos))
		(defq pdir (get :sp_dir *player_man*))
		(def *player_man* :sp_death :nil)
		(. *player_man* :sp_kill)
		(setq *player_man* (ManStance px py pdir))
		(. *layer_player* :add_front *player_man*))
	(clear-layer *layer_enemies*)
	(clear-layer *layer_missiles*)
	(setq *enemy_stack* (list)))

(defun battle-lost-state-update ()
	; delay for 96 frames before progressing to defend/field via title
	(if *player_man* (update-camera *player_man*))
	(++ *battle_state_count*)
	(when (>= *battle_state_count* 96)
		(defq next_level (case *game_level*
				(+game_level_field +game_level_defend)
				(+game_level_seige +game_level_field)
				(:t +game_level_field)))
		(setq *game_level* next_level)
		(goto-title +game_state_battle)))