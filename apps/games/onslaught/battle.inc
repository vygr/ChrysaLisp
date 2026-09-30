;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught battle state
;;;;;;;;;;;;;;;;;;;;;;;;

(defq +max_enemy_stack 16 *enemy_max* 6 *mine_count* 0 *mine_max* 3
	*current_map_idx* 1 *game_level* +game_level_field *battle_state_count* 0
	*enemy_army* 0 *enemy_count* 0 *enemy_spawn_delay* 1 *enemy_wizshot* 0
	*enemy_stack* (list) *enemy_banner_captured* :nil
	*man_banner* 1 *enemy_banner* 5
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

(defun get-spawn-surface (side w &optional h)
	; find solid ground surface point (x y) wide enough for w at visible screen edge on map
	(bind '(lx ly lw lh) (. *world_scroll* :get_relative *layer_land*))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(defq cam_x (/ (neg lx) *zoom*) cam_y (/ (neg ly) *zoom*)
		cam_w (/ ww *zoom*) cam_h (/ wh *zoom*)
		map_w (/ lw *zoom*) map_h (/ lh *zoom*)
		probe_x (if (= side 1)
			(max 0 (min (- map_w 1) (+ cam_x cam_w -1)))
			(max 0 (min (- map_w 1) cam_x)))
		trail_x (if (= side 1)
			(+ probe_x w -1)
			(- (+ probe_x 1) w))
		max_rows (max 1 (/ cam_h +tile_height))
		start_row (game-random max_rows)
		cam_top_grid (- cam_y (% cam_y +tile_height))
		sprite_h (ifn h 32 h)
		row_idx 0
		found_y :nil)
	(while (and (not found_y) (< row_idx max_rows))
		(defq cur_row (% (+ start_row row_idx) max_rows)
			y (+ cam_top_grid (* cur_row +tile_height)))
		(when (< y map_h)
			(defq f_lead (. *layer_land* :get_tile_flags probe_x y)
				f_trail (if (or (< trail_x 0) (>= trail_x map_w))
					+fmap_stand
					(. *layer_land* :get_tile_flags trail_x y)))
			(when (and (bits? f_lead +fmap_stand) (bits? f_trail +fmap_stand)
					   (not (bits? f_lead +fmap_climb)) (not (bits? f_trail +fmap_climb)))
				(defq f_above (. *layer_land* :get_tile_flags probe_x (- y 1)))
				(unless (or (bits? f_above +fmap_stand)
						   (< (- y sprite_h) cam_y))
					(setq found_y y))))
		(++ row_idx))
	(if found_y (list probe_x found_y) :nil))

(defun place-enemy-sprite (sp surface_x surface_y)
	; place sprite so its bottom leading edge matches (surface_x, surface_y) with 1 pixel showing
	(bind '(w h) (. sp :sp_get_size))
	(defq dir (get :sp_dir sp)
		x (if (= dir 1) (- (+ surface_x 1) w) surface_x)
		y (- surface_y h))
	(. sp :sp_set_pos x y)
	(list x y w h))

(defun spawn-enemy-right ()
	; spawn an enemy on the right side of the screen moving left
	(defq army_tbl (elem-get +army_tables *enemy_army*)
		lvl_offset (* *game_level* 6)
		unit_idx (+ (game-random 6) lvl_offset)
		type (elem-get army_tbl unit_idx))
	(bind '(w h) (enemy-size type))
	(when (defq pt (get-spawn-surface 1 w h))
		(bind '(px py) pt)
		(defq dir -1
			x px
			y (- py h)
			stationary (is-stationary-type type))
		(unless (or (and stationary (>= x (const (- (* +map_width +tile_width) 32))))
					(and stationary (sprite-collide *layer_enemies* x y w h
						(logior +ftp_balista +ftp_cannon +ftp_oil +ftp_beserk))))
			(defq sp (create-enemy-type type)
				mv (elem-get (get :sp_components sp) +en_comp_mv))
			(def sp :sp_dir dir)
			(when mv
				(set mv :vx (neg (get :max_vx mv))))
			(place-enemy-sprite sp px py)
			(. *layer_enemies* :add_front sp)
			(++ *enemy_count*))))

(defun spawn-enemy-left ()
	; spawn an overrun enemy from the stack on the left side moving right
	(if (empty? *enemy_stack*) (spawn-enemy-right)
		(defq type (last *enemy_stack*) pt :nil)
		(bind '(w h) (enemy-size type))
		(ifn (setq pt (get-spawn-surface -1 w h)) (spawn-enemy-right)
			(bind '(px py) pt)
			(defq dir 1
				x (- (+ px 1) w)
				y (- py h)
				stationary (is-stationary-type type))
			(if (or (and stationary (<= x 0))
					(and stationary (sprite-collide *layer_enemies* x y w h
						(logior +ftp_balista +ftp_cannon +ftp_oil +ftp_beserk))))
				(spawn-enemy-right)
				(pop *enemy_stack*)
				(defq sp (create-enemy-type type)
					mv (elem-get (get :sp_components sp) +en_comp_mv))
				(def sp :sp_dir dir)
				(when mv
					(set mv :vx (get :max_vx mv)))
				(place-enemy-sprite sp px py)
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
	; spawn ground mines ahead of player in *layer_bans* with 1 pixel showing
	(when (and (/= *game_level* +game_level_defend)
			   (< *mine_count* *mine_max*)
			   *player_man*)
		(defq dir (get :sp_dir *player_man*))
		(when (defq pt (get-spawn-surface dir 16 8))
			(bind '(px py) pt)
			(defq x (if (= dir 1) px (- (+ px 1) 16))
				y (- py 8))
			(unless (sprite-collide *layer_bans* (- x 28) y 72 8 +ftp_enemy_fire)
				(defq mine (Mine))
				(. mine :sp_set_pos x y)
				(. *layer_bans* :add_front mine)
				(++ *mine_count*)))))

;;;;;;;;;;;;;;;;;;;;
; battle state hooks
;;;;;;;;;;;;;;;;;;;;

(defun battle-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
	(. *layer_sky* :sky_random)
	(setq *game_level* +game_level_field
		  *current_map_idx* 1)
	(. *layer_land* :load_field_map 1)
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
		  *enemy_max* (if (= *menu_difficulty* 0) (+ (game-random 2) 2) (+ (game-random 4) 2))
		  *enemy_wizshot* (game-random 2)
		  *enemy_army* (game-random (length +army_tables)))
	; spawn banners
	(bind '(mx my ex ey) (case *game_level*
		(+game_level_seige '(16 192 2016 0))
		(+game_level_defend '(16 0 2016 192))
		(:t '(16 128 2016 128))))
	(setq *man_banner* (+ +frm_16x16_banners (game-random 16))
		  *enemy_banner* (+ +frm_16x16_banners (game-random 16)))
	(when (= *man_banner* *enemy_banner*)
		(setq *enemy_banner* (+ +frm_16x16_banners (% (+ (- *enemy_banner* +frm_16x16_banners) 1) 16))))
	(defq man_ban (Banner +ftp_man_banner *man_banner*))
	(. man_ban :sp_set_pos mx my)
	(. *layer_bans* :add_front man_ban)
	(defq enemy_ban (Banner +ftp_enemy_banner *enemy_banner*))
	(. enemy_ban :sp_set_pos ex ey)
	(. *layer_bans* :add_front enemy_ban)
	(update-camera *player_man*))

(defun battle-state-update ()
	; check for player death, battle won, or overrun stack full
	(cond
		((or (not *player_man*) (get :sp_dead *player_man*))
			(goto-title +game_state_menu))
		((>= (length *enemy_stack*) +max_enemy_stack)
			(setq *game_state* +game_state_battle_lost))
		((and *enemy_banner_captured*
			  (empty? *enemy_stack*)
			  (empty? (. *layer_enemies* :children)))
			(setq *game_state* +game_state_battle_won))
		(:t
			(init-mines)
			(init-enemies)
			(update-camera *player_man*))))

(defun battle-won-state-init ()
	(setq *battle_state_count* 0))

(defun battle-won-state-update ()
	; delay for 96 frames before returning to menu
	(++ *battle_state_count*)
	(when (>= *battle_state_count* 96)
		(goto-title +game_state_menu)))

(defun battle-lost-state-init ()
	(setq *battle_state_count* 0))

(defun battle-lost-state-update ()
	; delay for 96 frames before returning to menu
	(++ *battle_state_count*)
	(when (>= *battle_state_count* 96)
		(goto-title +game_state_menu)))