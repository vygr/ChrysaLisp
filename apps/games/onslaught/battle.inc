;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/battle.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught battle state
;;;;;;;;;;;;;;;;;;;;;;;;

(defq +max_enemy_stack 16 *enemy_max* 6
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

(defun get-spawn-xy (w h dir)
	; search downwards for a solid standable map platform
	(defq x (if (= dir 1) (+ *camera_x* +window_width -1) (- *camera_x* w -1))
		x (max 0 (min (- (* +map_width +tile_width) w) x))
		x1 (+ x w -1)
		rand_y (* (random (const (- (/ +window_height +tile_height) 1))) +tile_height)
		y (logand (+ *camera_y* rand_y) (neg +tile_height))
		cam_bottom (+ *camera_y* +window_height)
		found_y :nil)
	(while (and (not found_y) (< y (const (* +map_height +tile_height))))
		(defq f1 (. *layer_land* :get_tile_flags x y)
			f2 (. *layer_land* :get_tile_flags x1 y))
		(when (and (bits? f1 +fmap_stand) (bits? f2 +fmap_stand)
				   (not (bits? f1 +fmap_climb)) (not (bits? f2 +fmap_climb)))
			(defq sy (- y h))
			(if (<= *camera_y* sy (- cam_bottom 16))
				(setq found_y sy)
				(setq y (+ (const (* +map_height +tile_height)) 1))))
		(setq y (+ y +tile_height)))
	(if found_y (list x found_y) :nil))

(defun spawn-enemy-right ()
	; spawn an enemy on the right side of the screen moving left
	(defq army_tbl (elem-get +army_tables *enemy_army*)
		lvl_offset (* *game_level* 6)
		unit_idx (+ (random 6) lvl_offset)
		type (elem-get army_tbl unit_idx))
	(bind '(w h) (elem-get +enemy_sizes type))
	(when (defq pos (get-spawn-xy w h 1))
		(bind '(x y) pos)
		(defq mask (if (is-stationary-type type)
					   (logior +ftp_balista +ftp_cannon +ftp_oil +ftp_beserk)
					   -1))
		(unless (or (and (is-stationary-type type) (>= x (const (- (* +map_width +tile_width) 32))))
					(sprite-collide *layer_enemies* x y w h mask))
			(defq sp (create-enemy-type type)
				mv (elem-get (get :sp_components sp) +en_comp_mv))
			(def sp :sp_dir -1)
			(when mv
				(set mv :vx (neg (get :max_vx mv))))
			(. sp :sp_set_pos x y)
			(. *layer_enemies* :add_front sp)
			(++ *enemy_count*))))

(defun spawn-enemy-left ()
	; spawn an overrun enemy from the stack on the left side moving right
	(when (> *enemy_stack_index* 0)
		(defq type (last *enemy_stack*))
		(bind '(w h) (elem-get +enemy_sizes type))
		(ifn (defq pos (get-spawn-xy w h -1)) (spawn-enemy-right)
			(bind '(x y) pos)
			(defq mask (if (is-stationary-type type)
						   (logior +ftp_balista +ftp_cannon +ftp_oil +ftp_beserk)
						   -1))
			(if (sprite-collide *layer_enemies* x y w h mask)
				(spawn-enemy-right)
				(pop *enemy_stack*)
				(-- *enemy_stack_index*)
				(defq sp (create-enemy-type type)
					mv (elem-get (get :sp_components sp) +en_comp_mv))
				(def sp :sp_dir 1)
				(when mv
					(set mv :vx (get :max_vx mv)))
				(. sp :sp_set_pos x y)
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
		  *enemy_max* (+ (random 4) 2)
		  *enemy_army* (random (length +army_tables)))
	(update-camera *player_man*))

(defun battle-state-update ()
	; check for player death or overrun stack full
	(cond
		((or (not *player_man*) (get :sp_dead *player_man*))
			(goto-title +game_state_hiscore))
		((>= *enemy_stack_index* +max_enemy_stack)
			(setq *game_state* +game_state_battle_lost))
		(:t
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