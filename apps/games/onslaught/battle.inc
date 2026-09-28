;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/battle.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught battle state
;;;;;;;;;;;;;;;;;;;;;;;;

(defq *current_map_idx* 1 *game_level* +game_level_field *battle_state_count* 0)

; collision handlers for player man

(defun cl-man-hits-enemy (this hit)
	; man colliding with general enemy
	(defq frame (get :sp_frame this))
	(when (and (>= frame +frm_fanatic_l_climb) (<= frame (+ +frm_fanatic_l_climb 3)))
		(audio-play-rpc *sfx_clash1*)
		(. this :sp_kill)))

(defun cl-man-hits-missile (this hit)
	; missile colliding with man
	(audio-play-rpc *sfx_explode*)
	(. hit :sp_kill)
	(. this :sp_kill))

(defun cl-man-hits-banner (this hit)
	; banner capture or touch
	(audio-play-rpc *sfx_spell*))

; collision dispatch tables

(defq
	*man_missile_collision_table* (list (const cl-man-hits-missile))
	*man_enemy_collision_table* (list (const cl-man-hits-enemy))
	*man_banner_collision_table* (list (const cl-man-hits-banner)))

(defun battle-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
	(. *layer_sky* :sky_random)
	(. *layer_land* :load_field_map *current_map_idx*)
	(def (defq man (Fanatic)) :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0)
	(. man :sp_set_pos 8 208)
	(. *layer_player* :add_front man)
	(setq *player_man* man))

(defun battle-state-update ()
	; check for player death
	(if (or (not *player_man*) (get :sp_dead *player_man*))
		(goto-title +game_state_hiscore)
		(update-camera *player_man*)
		(update-sprite-layers)))

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