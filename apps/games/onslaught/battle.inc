;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/battle.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught battle state
;;;;;;;;;;;;;;;;;;;;;;;;

(defq +max_enemy_stack 16 *enemy_max* 6 *mine_count* 0 *mine_max* 3
	*current_map_idx* 1 *game_level* +game_level_field *battle_state_count* 0
	*enemy_army* 0 *enemy_count* 0 *enemy_spawn_delay* 1
	*enemy_stack* (list) *enemy_stack_index* 0
	+army_names
	''("HILLMEN" "NECROMANTIC" "ROBBER" "BOARRIDER" "MERCENARY"
	   "KNIGHTLY" "BERSERKER" "MYSTIC" "BALISTIC" "POWDER"
	   "CAULDRON" "MONASTIC" "JUGGERNAUT" "PLAGUE")
	+army_tables
	''(; 0: hillmen
	   (1 1 1 1 1 12 1 1 1 1 1 12 1 1 1 1 1 12)
	   ; 1: necromantic
	   (2 2 2 2 2 9 2 2 2 2 2 9 2 2 2 2 2 9)
	   ; 2: robber
	   (3 3 3 3 3 6 3 3 3 3 3 6 3 3 3 3 3 3)
	   ; 3: boarrider
	   (8 8 8 8 8 11 1 1 1 1 1 11 1 1 1 1 1 11)
	   ; 4: mercenary
	   (4 4 4 4 4 10 4 4 4 4 10 4 4 4 4 4 1 4)
	   ; 5: knightly
	   (0 0 0 0 0 8 4 4 4 4 4 1 4 4 4 4 4 1)
	   ; 6: berserker
	   (12 12 12 12 12 1 12 12 12 12 12 1 12 12 12 12 12 1)
	   ; 7: mystic
	   (9 9 9 9 9 10 9 9 9 9 10 9 9 9 9 9 9 1)
	   ; 8: balistic
	   (5 5 5 5 6 5 5 5 5 5 5 6 3 3 3 3 3 3)
	   ; 9: powder
	   (6 6 6 6 6 3 6 6 6 6 6 3 3 3 3 3 3 3)
	   ; 10: cauldron
	   (3 3 3 3 3 0 7 7 7 7 7 4 3 3 3 3 3 4)
	   ; 11: monastic
	   (11 11 11 11 11 12 11 11 11 11 12 11 11 11 11 11 12 11)
	   ; 12: juggernaut
	   (10 10 10 10 10 5 10 10 10 10 10 7 1 1 1 1 1 5)
	   ; 13: plague
	   (14 14 14 13 13 13 15 15 14 13 13 13 15 15 14 13 13 13)))

(defun is-stationary-type (type)
	; balista (5), cannon (6), oil (7), beserk (12)
	(or (= type 5) (= type 6) (= type 7) (= type 12)))

;;;;;;;;;;;;;;;;;;;;;;
; enemy spawn routines
;;;;;;;;;;;;;;;;;;;;;;

(defun get-spawn-surface (side w &optional h)
	; find solid ground surface point (x y) wide enough for w at visible screen edge on map
	(bind '(lx ly lw lh) (. *world_scroll* :get_relative *layer_land*))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(defq cam_x (/ (neg lx) *zoom*)
		cam_y (/ (neg ly) *zoom*)
		cam_w (/ ww *zoom*)
		cam_h (/ wh *zoom*)
		map_w (/ lw *zoom*)
		map_h (/ lh *zoom*)
		probe_x (if (= side 1)
			(max 0 (min (- map_w 1) (+ cam_x cam_w -1)))
			(max 0 (min (- map_w 1) cam_x)))
		trail_x (if (= side 1)
			(+ probe_x w -1)
			(- (+ probe_x 1) w))
		rand_y (* (random (max 1 (- (/ cam_h +tile_height) 1))) +tile_height)
		start_y (+ (- cam_y (% cam_y +tile_height)) rand_y)
		cam_bottom (+ cam_y cam_h)
		sprite_h (or h 32)
		y start_y
		found_y :nil)
	(while (and (not found_y) (< y cam_bottom) (< y map_h))
		(defq f_lead (. *layer_land* :get_tile_flags probe_x y)
			f_trail (if (or (< trail_x 0) (>= trail_x map_w))
				+fmap_stand
				(. *layer_land* :get_tile_flags trail_x y)))
		(when (and (bits? f_lead +fmap_stand) (bits? f_trail +fmap_stand)
				   (not (bits? f_lead +fmap_climb)) (not (bits? f_trail +fmap_climb)))
			(defq f_above (. *layer_land* :get_tile_flags probe_x (- y 1))
				f_above_t (if (or (< trail_x 0) (>= trail_x map_w))
					0
					(. *layer_land* :get_tile_flags trail_x (- y 1))))
			(unless (or (bits? f_above +fmap_stand) (bits? f_above_t +fmap_stand)
					   (< (- y sprite_h) cam_y))
				(setq found_y y)))
		(setq y (+ y +tile_height)))
	(if found_y (list probe_x found_y) :nil))

(defun place-enemy-sprite (sp surface_x surface_y)
	; place sprite so its bottom leading edge matches (surface_x, surface_y) with 1 pixel showing
	(bind '(w h) (. sp :sp_get_size))
	(defq dir (or (get :sp_dir sp) 1)
		x (if (= dir 1) (- (+ surface_x 1) w) surface_x)
		y (- surface_y h))
	(. sp :sp_set_pos x y)
	(list x y w h))

(defun spawn-enemy-right ()
	; spawn an enemy on the right side of the screen moving left
	(defq army_tbl (elem-get +army_tables *enemy_army*)
		lvl_offset (* *game_level* 6)
		unit_idx (+ (random 6) lvl_offset)
		type (elem-get army_tbl unit_idx))
	(print "spawn unit type =" type)
	(print)
	(bind '(w h) (elem-get +enemy_sizes type))
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
	(if (<= *enemy_stack_index* 0) (spawn-enemy-right)
		(defq type (last *enemy_stack*)
			pt :nil)
		(print "spawn left unit type =" type)
		(print)
		(bind '(w h) (elem-get +enemy_sizes type))
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
				(-- *enemy_stack_index*)
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
			(setq *enemy_spawn_delay* (+ (random 8) 8))
			(when *player_man*
				(defq pdir (or (get :sp_dir *player_man*) 1))
				(cond
					((= pdir 1)
						(spawn-enemy-right))
					(:t
						(if (> *enemy_stack_index* 0)
							(spawn-enemy-left)
							(spawn-enemy-right))))))))

(defun init-mines ()
	; spawn ground mines ahead of player in *layer_bans* with 1 pixel showing
	(when (and (/= *game_level* +game_level_defend)
			   (< *mine_count* *mine_max*)
			   *player_man*)
		(defq dir (or (get :sp_dir *player_man*) 1))
		(when (defq pt (get-spawn-surface dir 16 8))
			(bind '(px py) pt)
			(defq x (if (= dir 1) px (- (+ px 1) 16))
				y (- py 8))
			(unless (sprite-collide *layer_bans* (- x 28) y 72 8 +ftp_enemy_fire)
				(defq mine (Mine))
				(. mine :sp_set_pos x y)
				(. *layer_bans* :add_front mine)
				(++ *mine_count*)))))

;;;;;;;;;;;;;;;;;;;;;;
; battle state hooks
;;;;;;;;;;;;;;;;;;;;;;

(defun battle-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
	(. *layer_sky* :sky_random)
	(. *layer_land* :load_field_map *current_map_idx*)
	(def (defq man (Fanatic)) :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0)
	(. man :sp_set_pos 8 208)
	(. *layer_player* :add_front man)
	(setq *player_man* man
		  *enemy_stack* (list)
		  *enemy_stack_index* 0
		  *enemy_count* 0
		  *enemy_spawn_delay* 1
		  *mine_count* 0
		  *mine_max* (+ (random 3) 1)
		  *enemy_max* (+ (random 4) 2)
		  *enemy_army* 8)
	(print "army type =" *enemy_army* (elem-get +army_names *enemy_army*))
	(print)
	(update-camera *player_man*))

(defun battle-state-update ()
	; check for player death or overrun stack full
	(cond
		((or (not *player_man*) (get :sp_dead *player_man*))
			(goto-title +game_state_hiscore))
		((>= *enemy_stack_index* +max_enemy_stack)
			(setq *game_state* +game_state_battle_lost))
		(:t
			(init-mines)
			(init-enemies)
			(update-camera *player_man*)
			(update-sprite-layers))))

(defun battle-won-state-init ()
	(setq *battle_state_count* 0))

(defun battle-won-state-update ()
	; delay for 96 frames before level interlude
	(++ *battle_state_count*)
	(when (>= *battle_state_count* 96)
		(defq next_state (case *game_level*
			(+game_level_field
				(setq *game_level* +game_level_seige)
				+game_state_battle)
			(+game_level_seige
				+game_state_mind)
			(:t
				(setq *game_level* +game_level_field)
				+game_state_battle)))
		(goto-title next_state)))

(defun battle-lost-state-init ()
	(setq *battle_state_count* 0))

(defun battle-lost-state-update ()
	; delay for 96 frames before retreat interlude
	(++ *battle_state_count*)
	(when (>= *battle_state_count* 96)
		(defq next_state (case *game_level*
			(+game_level_field
				(setq *game_level* +game_level_defend)
				+game_state_battle)
			(+game_level_seige
				(setq *game_level* +game_level_field)
				+game_state_battle)
			(:t
				+game_state_mind)))
		(goto-title next_state)))